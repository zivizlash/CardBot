module CardBot.Types

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

type TableCard = {
    Card: Card
    PlayerIndex: int
}

type SessionPlayState = {
    Trump: Suit
    Cards: Card list
    Players: Player list
    Table: TableCard list
    TurnPlayerIndex: int
}

type SessionJoinState = {
    Players: int list
    LastJoinedId: int
}

type Session =
    | Join of SessionJoinState
    | Play of SessionPlayState

type SessionEndGameResult = {
    FoolPlayerId: int
    WinnerPlayerId: int
}

type PlayerTurn = { 
    PlayerId: int
    Card: Card
}

type PlayerTake = { PlayerId: int }
type PlayerLeave = { PlayerId: int }
type PlayerJoin = { PlayerId: int }

type PlayerJoinResult = { PlayerId: int }
type PlayerLeaveResult = { PlayerId: int }

type SessionCreation =
    | Created of SessionPlayState
    | TooManyPlayers
    | TooFewPlayers

type PlayerAction = 
    | Turn of PlayerTurn
    | Take of PlayerTake

type SessionAction = 
    | Start
    | Join of PlayerJoin
    | Player of PlayerAction
    | Leave of PlayerLeave

type GameCreatedResult = {
    PlayerTurn: int
    PlayerTo: int
    Trump: Suit
}

type NotPlayerTurnResult = {
    Player: int
}

type AttackerTurnResult = {
    PlayerTurn: int
    PlayerTo: int
}

type DefenderTurnResult = {
    DefenderId: int
}

type SessionActionResult = 
    | Continue
    | GameGreated of GameCreatedResult
    | PlayerJoined of PlayerJoinResult
    | PlayerLeaved of PlayerLeaveResult
    | AttackerTurn of AttackerTurnResult
    | DefenderTurn of DefenderTurnResult
    | NotPlayerTurn of NotPlayerTurnResult
    | End of SessionEndGameResult

type SessionUpdateResult = Session * SessionActionResult list
