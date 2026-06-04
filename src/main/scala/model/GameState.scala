package model

enum GamePhase:
  case Betting
  case PlayerTurn
  case DealerTurn
  case Resolved(outcome: Outcome)

enum Outcome:
  case PlayerWins, DealerWins, Push, PlayerBusts, DealerBusts, PlayerBlackjack

case class GameState(
  deck:       List[Card],
  playerHand: Hand,
  dealerHand: Hand,
  phase:      GamePhase,
  balance:    Int,
  bet:        Int,
  minBet:     Int = 1,
  maxBet:     Int = 400
)
