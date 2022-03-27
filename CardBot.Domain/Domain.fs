module CardBot.Domain
open CardBot.Types
open System

let allSuits = [| Diamonds; Clubs; Hearts; Spades |] 

let allRanks =
    seq {
        yield Ace
        yield King
        yield Queen
        yield Jack
        for i in 6..10 do yield Value(i)
    } |> Seq.toArray

let fullDeck =
    allRanks 
    |> Array.allPairs allSuits 
    |> Array.map (fun (suit, rank) -> { Rank = rank; Suit = suit })
    
let shuffle (cards: Card array) =
    let random = Random()
    let deck = Array.copy cards
    
    for i = 0 to deck.Length - 1 do
        let posB = random.Next(0, deck.Length)
        let temp = deck[i]
        deck[i] <- deck[posB]
        deck[posB] <- temp

    deck |> Array.toList

let pickupCards fullDeckSize (deck: Card list) (cards: Card list) =
    let clamp (value: int) min max = Math.Clamp(value, min, max)

    let needPick = fullDeckSize - List.length deck
    let canPick = clamp (List.length cards - needPick) 0 needPick
    let addedDeckCards, updatedCards = cards |> List.splitAt(canPick)
    (deck @ addedDeckCards, updatedCards)
    
let getDeckTrump deck = 
    let lastCard = deck |> List.last
    lastCard.Suit

let getPlayersDecks count cards =
    let addDeck (players, cards) _ =
        let deck, cards = cards |> pickupCards 6 [] 
        (deck::players, cards)
    
    List.fold addDeck ([], cards) (List.init count id)

let createSession (playerIds: int list) =
    match playerIds |> List.length with
    | playersCount when playersCount > 6 ->
        TooManyPlayers
    | playersCount when playersCount < 2 ->
        TooFewPlayers
    | playersCount ->
        let createPlayer (deck, id) = { Deck = deck; Id = id }

        let cards = fullDeck |> shuffle 
        let trump = cards |> getDeckTrump
        let decks, cards = cards |> getPlayersDecks playersCount

        let players = 
            playerIds 
            |> List.sort 
            |> List.zip decks 
            |> List.map createPlayer

        Created <| { 
            SessionPlayState.Trump = trump
            Table = []
            Cards = cards
            Players = players
            TurnPlayerIndex = 0 
        }
        
let isTrump (session: SessionPlayState) card = session.Trump = card.Suit

let getRankValue card = 
    match card.Rank with
    | Value v -> v
    | Jack -> 11
    | Queen -> 12
    | King -> 13
    | Ace -> 14

let canBeat session striking target =
    let canValueBeat striking target = 
        getRankValue striking > getRankValue target

    let isTrump = isTrump session

    match isTrump striking, isTrump target with
    | true, false -> true
    | false, true -> false
    | _ -> canValueBeat striking target

let play handler session (actions: SessionAction list) = 
    let apply state action = 
        let session, actions = state
        let updatedSession, newActions = handler session action
        let actionResult = actions @ newActions
        SessionUpdateResult(updatedSession, actionResult)

    let initial = SessionUpdateResult(session, [])
    List.fold apply initial actions
