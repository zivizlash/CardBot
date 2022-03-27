module CardBot.Utils

open CardBot
open Funogram
open Funogram.Types
open Funogram.Telegram.Bot
open Funogram.Telegram.Types
open System.Collections.Concurrent
open System.Collections.Generic

type TelegramMessageContent = 
    | Text of string
    | Sticker of Sticker
    | Inline of InlineQuery
    | ChosenInline of ChosenInlineResult

let simpleTryGetValue (dictionary: IDictionary<'a, 'b>) key = 
    let mutable value = Unchecked.defaultof<'b>
    let refValue = ref value

    if dictionary.TryGetValue(key, refValue) then 
        Option.Some refValue.Value
    else 
        Option.None

let tryGetValue (dictionary: ConcurrentDictionary<'a, 'b>) key = 
    let mutable value = Unchecked.defaultof<'b>
    let refValue = ref value

    if dictionary.TryGetValue(key, refValue) then 
        Option.Some refValue.Value
    else 
        Option.None

type TelegramResponses(config: Types.BotConfig) = 
    let responses = List<unit -> Async<ApiResponseError option>>()
    let wrap (arg: IRequestBase<'a>) = 
        fun () ->
            async { 
                let! response = Api.api config arg
                return
                    match response with
                    | Ok _ -> Option.None
                    | Error apiError -> Option.Some apiError
            }
    member _.Add(message: IRequestBase<'a>) = responses.Add(wrap message)
    member _.SendAsync() = 
        async {
            let! errors = 
                responses
                |> Seq.map (fun call -> call())
                |> Async.Sequential
            return 
                errors
                |> Seq.where Option.isSome
                |> Seq.map Option.get
        }

let getUsername (user: User) = 
    match user.Username with
    | Some username -> 
        sprintf "@%s" username
    | Option.None -> 
        Option.fold (sprintf "%s %s") user.FirstName user.LastName

let getTextMessage (ctx: UpdateContext) = 
    ctx.Update.Message
    |> Option.bind (fun message -> message.Text)
    |> Option.map (fun text -> text.Trim())
     
let getSticker (ctx: UpdateContext) = 
    ctx.Update.Message
    |> Option.bind (fun message -> message.Sticker)

let getInlineQuery (ctx: UpdateContext) = 
    ctx.Update.InlineQuery

let getChosenInline (ctx: UpdateContext) = 
    ctx.Update.ChosenInlineResult

let getMessageContent (ctx: UpdateContext) = 
    match getTextMessage ctx, getSticker ctx, getInlineQuery ctx, getChosenInline ctx with
    | Some text, _, _, _ -> Some(Text text)
    | _, Some sticker, _, _ -> Some(Sticker sticker)
    | _, _, Some query, _ -> Some(Inline query)
    | _, _, _, Some chosenQuery -> Some(ChosenInline chosenQuery)
    | _ -> Option.None

let getUser (ctx: UpdateContext) = 
    ctx.Update.Message 
    |> Option.bind (fun msg -> msg.From)
    |> Option.orElseWith 
        (fun () -> ctx.Update.InlineQuery |> Option.map (fun query -> query.From))
        
let getUserId (ctx: UpdateContext) =
    getUser ctx |> Option.map (fun user -> user.Id)
        
let getChatId (ctx: UpdateContext) =
    ctx.Update.Message |> Option.map (fun msg -> msg.Chat.Id)        

let private cardToIdentiferDict =   
    let getCardIdentifier (card: CardBot.Types.Card) = 
        sprintf "%A|%A" card.Rank card.Suit

    Domain.fullDeck
    |> Seq.map (fun card -> (getCardIdentifier card, card))
    |> dict

let private identifierToCardDict = 
    cardToIdentiferDict
    |> Seq.map (fun (kv) -> kv.Value, kv.Key)
    |> dict

let getCardUid card = 
    simpleTryGetValue identifierToCardDict card
    
let getCardByIdentifier uid = 
    simpleTryGetValue cardToIdentiferDict uid
    
let play = Domain.play Handlers.handleAction
