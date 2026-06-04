package model

enum GamePhase:
  case Betting
  case Insurance   // dealer up-card is Ace: player may take insurance
  case PlayerTurn
  case Resolved    // round over; per-hand results live in GameState.hands

enum Outcome:
  case PlayerWins, DealerWins, Push, PlayerBusts, DealerBusts, PlayerBlackjack, Surrender

/** One player hand. After a split a round holds several. */
case class PlayerHand(
  cards:   Hand,
  bet:     Int,
  done:    Boolean         = false,
  outcome: Option[Outcome] = None
)

case class GameState(
  deck:       List[Card],
  hands:      List[PlayerHand],
  active:     Int,
  dealerHand: Hand,
  phase:      GamePhase,
  balance:    Int,
  bet:        Int,        // working bet during the betting phase
  insurance:  Int = 0     // insurance side bet
):
  def activeHand: PlayerHand   = hands(active)
  def stake:      Int          = if hands.isEmpty then bet else hands.map(_.bet).sum + insurance
