module CardBot.Domain

open System
open Funogram.Telegram.Bot

type Suit =
    | Diamonds
    | Clubs
    | Hearts
    | Spades
    
type Rank =
    | Ace
    | King
    | Queen
    | Jack
    | Value of int
    
type Card = {
    Suit: Suit
    Rank: Rank
}

type Player = {
    Id: int
    Deck: Card list
}

type Session = {
    Trump: Suit
    Cards: Card list
    Players: Player list
    Turn: int
}

type SessionCreation =
    | Session of Session
    | TooManyPlayers
    | TooFewPlayers

let allSuits =
    [ Diamonds; Clubs; Hearts; Spades ]
    |> List.toArray

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
    let length = cards.Length
    
    let sortByRandom = Seq.sortBy(fun _ -> random.Next(0, length))
    
    let rec repeat func times data =
        if times > 0 then
            repeat func (times - 1) (func data)
        else
            data
    
    fullDeck
    |> Array.toSeq
    |> repeat sortByRandom 2 
    |> Seq.toList

let pickupCards fullDeckSize (deck: Card list) (cards: Card list) =
    let clamp (value: int) min max = Math.Clamp(value, min, max)
    
    let needPick = fullDeckSize - List.length deck
    let canPick = clamp (List.length cards - needPick) 0 needPick;
    let addedDeckCards, updatedCards = cards |> List.splitAt(canPick)
    (deck @ addedDeckCards, updatedCards)

let getPlayersDecks cards count =
    let addDeck players cards =
        let deck, cards = pickupCards 6 [] cards
        (deck::players, cards)
        
    let wrapper (players, cards) _ =
        addDeck players cards
    
    List.fold wrapper ([], cards) (List.init count id)

let createSession playersCount =
    let pickupTrump cards =
        let trump, cards = pickupCards 1 [] cards
        ((trump |> List.head).Suit, cards)
        
    match playersCount with
    | count when count > 6 ->
        TooManyPlayers
    | count when count < 2 ->
        TooFewPlayers
    | _ ->
        let trump, cards = shuffle fullDeck |> pickupTrump
        let playersDecks, cards = getPlayersDecks cards playersCount
        let players = playersDecks |> List.mapi (fun id deck -> { Deck = deck; Id = id })
        Session { Trump = trump; Cards = cards; Players = players; Turn = 0 }
