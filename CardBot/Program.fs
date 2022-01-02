open Funogram.Telegram
open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Types
open Funogram.Telegram.RequestsTypes
open Funogram.Telegram.Bot
open Funogram.Telegram.Api
open CardBot.Domain

let bot config data =
    api config data |> Async.RunSynchronously |> ignore

let onMessage (ctx: UpdateContext) func =
    match ctx.Update.Message with
    | Some msg -> func msg
    | None -> ()

let onStart config (ctx: UpdateContext) =
    let processMessage msg =
        let shuffled = shuffle fullDeck
        ()
    
    onMessage ctx (fun msg -> bot config ("Hi" |> sendMessage msg.Chat.Id))

let onHelp config (ctx: UpdateContext) =
    onMessage ctx (fun msg -> bot config ("Help: nope" |> sendMessage msg.Chat.Id))

let onUpdate config (ctx: UpdateContext) =
    processCommands ctx [
        cmd "/start" (onStart config)
        cmd "/help" (onHelp config)
    ] |> ignore
    ()
    
[<EntryPoint>]
let main argv =
    let config = { defaultConfig with Token = "543452756:AAE8fno1i8xFMc-eCKSLunnJe1CEef9XzK8" }
    
    startBot config (onUpdate config) None
        |> Async.RunSynchronously
        
    0 // return an integer exit code
    