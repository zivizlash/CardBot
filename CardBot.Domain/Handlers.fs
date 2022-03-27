module CardBot.Handlers

open CardBot.Types
open CardBot.Domain

let findPlayerByIndex (index: int) (session: SessionPlayState) = 
    let searchedIndex = index % session.Players.Length
    session.Players |> List.item searchedIndex

let private wrap func value session = 
    let session, result = func session value
    SessionUpdateResult(session, [result])

let private emptyUpdate session = 
    SessionUpdateResult(session, [])

let isDefender (session: SessionPlayState) playerId = 
    playerId = (findPlayerByIndex (session.TurnPlayerIndex + 1) session).Id

let isAttacker (session: SessionPlayState) playerId = 
    playerId = (findPlayerByIndex session.TurnPlayerIndex session).Id

let getDefender (session: SessionPlayState) =
    findPlayerByIndex (session.TurnPlayerIndex + 1) session

let isEmptyOrSameCardValue (cards: TableCard list) (current: Card) = 
    cards.IsEmpty
    // ToDo: доделать для подкидного // cards |> List.forall(fun c -> c.Card.Rank = current.Rank)   

let handlerPlayerAction (session: SessionPlayState) (player: PlayerAction) =     
    let updatedSession, result = 
        match player with
        // turn - непроверенные данные полученные оберткой над tg
        | Turn turn -> 
            let unchangedSession = (session, NotPlayerTurn({ Player = turn.PlayerId }))

            let removeCardFromHand playerIndex card (session: SessionPlayState)  = 
                let player = session.Players[playerIndex]
                let cardList = player.Deck |> List.except [card]
                let players = 
                    session.Players |> List.updateAt playerIndex { player with Deck = cardList }
                { session with Players = players }

            let updateSessionTable (session: SessionPlayState) table = 
                { session with Table = table }
    
            let addCardToTable (turn: PlayerTurn) (session: SessionPlayState) =
                updateSessionTable session ({ Card = turn.Card; PlayerIndex = turn.PlayerId }::session.Table)

            let removeFirstTableCard (session: SessionPlayState) = 
                session.Table |> List.removeAt 0 |> updateSessionTable session

            let handleAttacker() = 
                if isEmptyOrSameCardValue session.Table turn.Card then
                    let updatedSession = 
                        session 
                        |> removeCardFromHand turn.PlayerId turn.Card
                        |> addCardToTable turn
                    
                    updatedSession, DefenderTurn { DefenderId = session.TurnPlayerIndex + 1 }
                else 
                    unchangedSession

            let handleDefender() = 
                let beat card = 
                    if canBeat session turn.Card card then
                        let updatedSession = 
                            session 
                            |> removeCardFromHand turn.PlayerId turn.Card
                            |> removeFirstTableCard

                        Some (updatedSession, Continue)
                    else
                        None

                session.Table 
                |> List.tryHead
                |> Option.map (fun tableCard -> tableCard.Card)
                |> Option.bind beat
                |> Option.defaultValue unchangedSession

            match isAttacker session turn.PlayerId, isDefender session turn.PlayerId with
            | true, _ -> handleAttacker()
            | _, true -> handleDefender()
            | _ -> session, NotPlayerTurn { Player = turn.PlayerId }
        | Take take -> 
            match session.Table.IsEmpty, isDefender session take.PlayerId with
            | false, true -> 
                

                session, Continue
            | _ -> session, NotPlayerTurn { Player = take.PlayerId }

    Play updatedSession, result

let handleLeaveAction (session: SessionPlayState) (leave: PlayerLeave) =    
    Play session, Continue

let handleAddPlayer (join: SessionJoinState) (playerJoin: PlayerJoin) = 
    let players = playerJoin.PlayerId::join.Players 
    let session = Session.Join { join with Players = players }
    session, SessionActionResult.PlayerJoined { PlayerId = playerJoin.PlayerId }

let handleCardRequest (play: SessionPlayState) (playerId: int) = 
    play.Players |> List.tryFind (fun player -> player.Id = playerId)

let handleRemovePlayer (join: SessionJoinState) (playerLeave: PlayerLeave) =
    let index = 
        join.Players |> List.tryFindIndex (fun id -> id = playerLeave.PlayerId)

    match index with
    | Some index -> 
        let session = Session.Join { 
            join with Players = join.Players |> List.removeAt index
        }
        session, SessionActionResult.PlayerJoined { PlayerId = playerLeave.PlayerId }
    | _ -> 
        Session.Join join, SessionActionResult.Continue

let handleStart (session: SessionJoinState) _ = 
    match session.Players |> createSession with
    | Created session -> 
        let playerTurn = findPlayerByIndex session.TurnPlayerIndex session 
        let playerTo = findPlayerByIndex (session.TurnPlayerIndex + 1) session
        
        Session.Play session, SessionActionResult.GameGreated {
            Trump = session.Trump
            PlayerTurn = playerTurn.Id
            PlayerTo = playerTo.Id
        }
    | _ -> Session.Join session, SessionActionResult.Continue

let handlePlayState (play: SessionPlayState) (action: SessionAction) = 
    let handler = 
        match action with
        | Player player -> wrap handlerPlayerAction player
        | Leave leave -> wrap handleLeaveAction leave
        | _ -> Session.Play >> emptyUpdate

    handler play

let handleJoinState (join: SessionJoinState) (action: SessionAction) = 
    let handler = 
        match action with 
        | Join playerJoin -> wrap handleAddPlayer playerJoin
        | Leave playerLeave -> wrap handleRemovePlayer playerLeave
        | Start -> wrap handleStart ()
        | _ -> Session.Join >> emptyUpdate

    handler join

let handleAction session (action: SessionAction) =
    let result = 
        match session with
        | Session.Join joinState -> handleJoinState joinState
        | Play playState -> handlePlayState playState

    result action
