package model

enum GamePhase:
  case PlayerTurn
  case DealerTurn
  case Resolved(outcome: Outcome)

enum Outcome:
  case PlayerWins, DealerWins, Push, PlayerBusts, DealerBusts, PlayerBlackjack

case class GameState(
  deck:       List[Card],
  playerHand: Hand,
  dealerHand: Hand,
  phase:      GamePhase
)
