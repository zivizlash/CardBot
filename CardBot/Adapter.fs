module CardBot.Adapter

open Funogram
open Funogram.Types
open Funogram.Telegram.Bot
open Funogram.Telegram.Types
open Funogram.Telegram.Api
open System.Collections.Concurrent
open System.Collections.Generic
open Funogram.Telegram
open CardBot.Utils
open CardBot.AdapterTypes

let sessionJoinState = Types.Join { Players = []; LastJoinedId = 0 }

let parseCommand getCardBySticker (ctx: UpdateContext) = 
    let parseSticker userId sticker = 
        match getCardBySticker sticker with
        | Some card -> Turn { TelegramPlayerId = userId; Card = card }
        | Option.None -> None

    let parseText user text = 
        match text with
        | "/game" -> Start
        | "/join" -> Join user
        | "/take" -> TelegramCommands.Take user 
        | "/exit" -> Exit { TelegramPlayerId = user.Id }
        | _ -> None

    let parseInlineQuery (user: User) (query: InlineQuery) = 
        CardsRequest { TelegramPlayerId = user.Id; RequestId = query.Id }

    let parse user = 
        match getMessageContent ctx with
        | Some (TelegramMessageContent.Text text)-> 
            parseText user text
        | Some (TelegramMessageContent.Sticker sticker) -> 
            parseSticker user.Id sticker
        | Some (TelegramMessageContent.Inline query) -> 
            parseInlineQuery user query
        | _ -> None

    getUser ctx |> Option.map parse

// При ливе игрока стейт сбивается
let getNewPlayerJoinId (session: Types.Session) = 
    match session with
    | Types.Join join when join.Players.Length <= 6 -> 
        if join.Players |> List.length > 0 then
            join.Players |> List.max |> (+) 1 |> Some
        else
            Some 0
    | _ -> Option.None
    
let executeCommand (binder: ITelegramSessionBinder) (session: Types.Session) (command: TelegramCommands) = 
    let formatAllPlayers() = 
        binder.Players 
        |> Seq.map (fun player -> sprintf "%i %i %s" player.Id player.TelegramId player.Name)
        |> Seq.fold (sprintf "%s%s\n") ""

    let processResults (results: Types.SessionActionResult list) = 
        let processToMessage actionResult = 
            match actionResult with
            | Types.Continue -> "To be continued..."
            | Types.PlayerJoined join ->
                formatAllPlayers()
            | Types.PlayerLeaved leave -> 
                formatAllPlayers()
            | Types.GameGreated created -> 
                let playerTurn = binder.FindByInternalId created.PlayerTurn
                let playerTo = binder.FindByInternalId created.PlayerTo
                match playerTo, playerTurn with
                | Some playerTurn, Some playerTo -> 
                    sprintf "Игра началась. Козырь: %A; Ходит: %s на %s" created.Trump playerTurn.Name playerTo.Name
                | _ -> ""
            | Types.End sessionEnd -> "Конец игры"
            | Types.AttackerTurn attackerTurn -> "Атака"
            | Types.DefenderTurn defenderTurn -> "Защита"
            | Types.NotPlayerTurn notPlayerTurn -> "Сейчас не ваш ход"

        results |> List.map processToMessage

    let getPlayerAndExecute playerId (func: TelegramBindedPlayer -> Types.SessionAction list) = 
        binder.FindByTelegramId playerId
        |> Option.map func
        |> Option.defaultValue []

    let actions = 
        match command with
        | Start -> [Types.Start]
        | Join join -> 
            match session |> getNewPlayerJoinId with
            | Some id -> 
                if binder.Bind(join, id) then 
                    [Types.SessionAction.Join { PlayerId = id }]
                else 
                    []
            | Option.None -> []
        | Take take -> 
            let getAction (player: TelegramBindedPlayer) = 
                let take = Types.PlayerAction.Take { PlayerId = player.Id }
                [Types.SessionAction.Player take]

            getPlayerAndExecute take.Id getAction
        | Turn turn ->
            let getAction (player: TelegramBindedPlayer) = 
                let turn = Types.PlayerAction.Turn { PlayerId = player.Id; Card = turn.Card }
                [Types.SessionAction.Player turn]

            getPlayerAndExecute turn.TelegramPlayerId getAction
        | Exit exit -> 
            let getAction (player: TelegramBindedPlayer) = 
                [Types.SessionAction.Leave { PlayerId = player.Id }]

            getPlayerAndExecute exit.TelegramPlayerId getAction
        | _ -> 
            []

    let updatedSession, results = play session actions

    let processedActions = 
        match command with 
        | CardsRequest request -> 
            match updatedSession, binder.FindByTelegramId(request.TelegramPlayerId) with
            | Types.Play play, Some player -> 
                match Handlers.handleCardRequest play player.Id with
                | Some player -> 
                    [Inline { RequestId = request.RequestId; Deck = player.Deck }]
                | _ -> []
            | _ -> []
        | _ -> []
    
    updatedSession, processedActions @ (results |> processResults |> List.map Text)

let rec mailboxHandler (options: TelegramMailboxStatic) session (processor: MailboxProcessor<MailboxMessage>) =
    async {
        let! msg = processor.TryReceive options.TimeoutMs

        match msg with
        | Some (Message ctx) -> 
            let response = options.CreateResponse()
            let updatedSession, messageResult = options.Update session ctx
   
            let addMessage message = 
                match message with
                | Text text -> response.Add(sendMessage options.ChatId text)
                | Inline request -> 
                    let processRequest card = 
                        match options.GetStickerFileId card, Utils.getCardUid card with
                        | Some fileId, Some cardId ->
                            Some (CachedSticker { 
                                InlineQueryResultCachedSticker.Id = cardId
                                StickerFileId = fileId
                                InputMessageContent = Option.None
                                ReplyMarkup = Option.None
                            })
                        | _ -> Option.None

                    let results = 
                        request.Deck 
                        |> Seq.map processRequest
                        |> Seq.filter Option.isSome
                        |> Seq.map Option.get
                        |> Seq.toArray

                    let answerInline = answerInlineQueryBase request.RequestId results (Some 5) (Some true) Option.None Option.None Option.None
                    response.Add(answerInline)

            messageResult |> List.iter addMessage

            let! _ = response.SendAsync()
            return! mailboxHandler options updatedSession processor
        | _ -> 
            return ()
    }

let createMailbox botConfig executor chatId (settings: TelegramSettings) session = 
    let update (s: Types.Session) (ctx: UpdateContext) = 
        ctx
        |> parseCommand settings.GetCardBySticker
        |> Option.map (fun command -> executor s command)
        |> Option.defaultValue (s, [])
    
    let mailboxData = {
        TimeoutMs = settings.TimeoutMs
        ChatId = chatId
        CreateResponse = fun () -> TelegramResponses botConfig
        Update = update
        GetStickerFileId = settings.GetStickerByCard
    }

    MailboxProcessor.Start (mailboxHandler mailboxData session)

type TelegramSessionChat(chatId: int64, createMailbox: TelegramSessionChat -> MailboxProcessor<MailboxMessage>) as self = 
    let chatId = chatId
    let TelegramIdToPlayers = ConcurrentDictionary<int64, TelegramBindedPlayer>()
    let createMailbox() = createMailbox self
    let mailbox = System.Lazy<_>(createMailbox, true)
    member _.ChatId = chatId
    member _.Players = TelegramIdToPlayers
    member _.FindByTelegramId(telegramPlayerId: int64) = tryGetValue TelegramIdToPlayers telegramPlayerId
    member _.RemovePlayer(playerId: int64) = TelegramIdToPlayers.TryRemove(playerId)
    member _.AddPlayer(user: User, internalId: int) = 
        let player = { Name = getUsername user; TelegramId = user.Id; Id = internalId }
        TelegramIdToPlayers.TryAdd(user.Id, player)
    member _.Process(ctx: UpdateContext) = mailbox.Value.Post (Message ctx)
    member _.Destroy() = mailbox.Value.Post Cancel
    
type TelegramSessionBinder(config: BotConfig, settings: TelegramSettings, 
    createBinder: TelegramSessionBinder -> TelegramSessionChat -> int64 -> ITelegramSessionBinder) as self = 

    let sessions = ConcurrentDictionary<int64, TelegramSessionChat>()
    let users = ConcurrentDictionary<int64, int64>()
    let binder chatId = fun session -> createBinder self session chatId
    
    let createMailbox chatId = 
        let binder = binder chatId
        fun chat -> createMailbox config (executeCommand (binder chat)) chatId settings sessionJoinState
        
    let getOrCreateSession chatId = 
        sessions.GetOrAdd(chatId, fun chatId -> TelegramSessionChat(chatId, createMailbox chatId))
    let unbindUser chatId userId = 
        users.TryRemove(KeyValuePair(userId, chatId))
    member _.BindUserToChat(chatId: int64, userId: int64) = 
        users.GetOrAdd(userId, chatId) = chatId
    member _.UnbindUserFromChat(chatId: int64, userId: int64) = 
        unbindUser chatId userId
    member _.GetOrCreateSession(chatId: int64) = 
        getOrCreateSession chatId
    member _.TryGetSessionFromUser(userId: int64) = 
        tryGetValue users userId |> Option.map getOrCreateSession
    member _.RemoveSession(chatId: int64) =
        tryGetValue sessions chatId 
        |> Option.map (fun session -> session.Players.Keys)
        |> Option.iter(Seq.iter (unbindUser chatId >> ignore))

let createBinderWrapper (binder: TelegramSessionBinder) (chatInfo: TelegramSessionChat) chatId = 
    { new ITelegramSessionBinder with
        member _.Bind(player: User, internalId: int) = 
            if chatInfo.AddPlayer(player, internalId) && binder.BindUserToChat(chatId, player.Id) then true
            else chatInfo.RemovePlayer player.Id |> ignore; false
        member _.Unbind(playerId: int64) = 
            let success = fst <| chatInfo.RemovePlayer(playerId)
            binder.UnbindUserFromChat(chatId, playerId) && success
        member _.Players = chatInfo.Players.Values
        member _.FindByTelegramId (id: int64) = 
            chatInfo.FindByTelegramId id
        member _.FindByInternalId (id: int) = 
            chatInfo.Players.Values |> Seq.tryFind (fun player -> player.Id = id)
        member _.ChatId = chatInfo.ChatId }

type TelegramSessions(config: BotConfig, settings: TelegramSettings) = 
    let binder = TelegramSessionBinder(config, settings, createBinderWrapper)
    member _.HandleMessage(ctx: UpdateContext) =  
        let sessionFromUser = getUserId ctx |> Option.bind binder.TryGetSessionFromUser
        let sessionByChat = getChatId ctx |> Option.map binder.GetOrCreateSession 
        let processMessage (session: TelegramSessionChat) = session.Process ctx

        sessionByChat 
        |> Option.orElse sessionFromUser 
        |> Option.iter processMessage
