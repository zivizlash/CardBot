module CardBot.Config

open Funogram.Types
open Funogram.Telegram.Bot

let config = { 
    defaultConfig with 
        Token = "" 
        AllowedUpdates = Some []
}
