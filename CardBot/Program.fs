open System.IO
open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Types
open Funogram.Telegram.Bot
open Funogram.Telegram.Api
open FSharp.Json
open CardBot.Types
open CardBot.Domain
open CardBot.Handlers
open CardBot.AdapterTypes

let config = { 
    defaultConfig with Token = "543452756:AAE8fno1i8xFMc-eCKSLunnJe1CEef9XzK8" 
}

type StickerCard = {
    Card: CardBot.Types.Card
    StickerUniqueFileId: string
    StickerFileId: string
}

let format card = sprintf "suit: %A; rank: %A" card.Suit card.Rank

let bot data = api config data |> Async.RunSynchronously |> ignore

let sendText text (ctx: UpdateContext) = 
    let send msg = bot (sendMessage msg.Chat.Id text)
    Option.iter send ctx.Update.Message

let cards = 
    File.ReadAllText("cards_collection.json") |> Json.deserialize<StickerCard list>

let findCardById fileId = 
    cards |> List.tryFind (fun card -> card.StickerUniqueFileId = fileId)

let findFileIdByCard (card: CardBot.Types.Card) = 
    cards 
    |> List.tryFind (fun stickerCard -> stickerCard.Card = card)
    |> Option.map (fun card -> card.StickerFileId)

let getSticker msg = msg.Sticker
let getUniqueFileId sticker = sticker.FileUniqueId
let getCardByStickerCard stickerCard = stickerCard.Card

let getCardBySticker = 
    getUniqueFileId >> findCardById >> Option.map getCardByStickerCard

let telegramSettings = {
    TelegramSettings.GetCardBySticker = getCardBySticker
    GetStickerByCard = findFileIdByCard
    TimeoutMs = 300000
}

let requiredCards = 
    CardBot.Domain.fullDeck
    |> Seq.toArray

let sessions = CardBot.Adapter.TelegramSessions(config, telegramSettings)

//let mutable cardPosition = 0
//let mutable stickerCards: StickerCard list = []
    
//let memorizeCards (ctx: UpdateContext) = 
    //let currentCardPosition = requiredCards[cardPosition % requiredCards.Length]
    //cardPosition <- cardPosition + 1
    
    //let formatStickerCard stickerCard = stickerCard.Card |> format
    //let sendCard text = sendText text ctx
    
    //let addCardToCollection (sticker: Sticker) = 
    //    let stickerCard = 
    //        { Card = currentCardPosition
    //          StickerUniqueFileId = sticker.FileUniqueId
    //          StickerFileId = sticker.FileId }

    //    stickerCards <- stickerCard::stickerCards
    //    System.IO.File.WriteAllText("cards_collection.json", Json.serialize stickerCards)
        
    //    format requiredCards[cardPosition % requiredCards.Length] |> sendCard

    //let sticker = 
    //    ctx.Update.Message
    //    |> Option.bind getSticker

    //sticker
    //|> Option.iter addCardToCollection
    
    //sticker
    //|> Option.map getUniqueFileId
    //|> Option.iter (fun v -> sendText v ctx)
    
    //sticker
    //|> Option.map (fun sticker -> sticker.FileId)
    //|> Option.iter (fun v -> sendText v ctx)

    //sticker
    //|> Option.map getUniqueFileId
    //|> Option.bind findCardById
    //|> Option.map formatStickerCard
    //|> Option.iter sendCard

let onUpdate (ctx: UpdateContext) =
    sessions.HandleMessage ctx
    // memorizeCards ctx
    
[<EntryPoint>]
let main argv =
    printfn "Starting bot..."
    startBot config onUpdate Option.None |> Async.RunSynchronously
    0
