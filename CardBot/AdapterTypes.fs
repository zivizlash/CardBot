module CardBot.AdapterTypes

open Funogram
open Funogram.Telegram.Bot
open Funogram.Telegram.Types
open Funogram.Telegram
open CardBot.Utils

type StickerCard = {
    Card: CardBot.Types.Card
    StickerUniqueFileId: string
    StickerFileId: string
}

type TelegramPlayer = {
    Name: string 
    TelegramId: int64
}

type TelegramBindedPlayer = {
    Name: string
    TelegramId: int64
    Id: int
}

type TelegramSessionInfo = {
    ChatId: int64
    Players: TelegramPlayer seq
}

type TelegramSessionState = 
    | Joining of TelegramSessionInfo
    | Playing of TelegramSessionInfo

type MailboxMessage = 
    | Message of UpdateContext
    | Cancel

type TelegramPlayerTurn = {
    Card: Types.Card
    TelegramPlayerId: int64
}

type TelegramPlayerInfo = {
    TelegramPlayerId: int64
}

type TelegramSettings = {
    GetCardBySticker: Sticker -> Types.Card option
    GetStickerByCard: Types.Card -> string option
    TimeoutMs: int
}

type TelegramCardsRequest = {
    TelegramPlayerId: int64
    RequestId: string
}

type TelegramCardsResponse = {
    RequestId: string
    Deck: Types.Card list
}

type TelegramCommands = 
    | None
    | Start
    | Join of User
    | Take of User
    | Turn of TelegramPlayerTurn
    | Exit of TelegramPlayerInfo
    | CardsRequest of TelegramCardsRequest
    
type TelegramMessageRequest = 
    | Text of string
    | Inline of TelegramCardsRequest

type TelegramMessageResponse = 
    | Text of string
    | Inline of TelegramCardsResponse

type ITelegramSessionBinder = 
    abstract member Bind: User * int -> bool
    abstract member Unbind: int64 -> bool
    abstract member FindByTelegramId: int64 -> TelegramBindedPlayer option
    abstract member FindByInternalId: int -> TelegramBindedPlayer option
    abstract member Players: TelegramBindedPlayer seq
    abstract member ChatId: int64

type TelegramMailboxStatic = {
    TimeoutMs: int
    ChatId: int64
    Update: Types.Session -> UpdateContext -> Types.Session * TelegramMessageResponse list
    CreateResponse: unit -> TelegramResponses
    GetStickerFileId: Types.Card -> string option
}

type TelegramAction = 
    | DomainAction of Types.SessionAction
    | InternalActionResult of TelegramMessageResponse
