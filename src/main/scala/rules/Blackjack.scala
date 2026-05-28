package rules

import cats.data.State
import model.*
import model.Hand.*
import scala.annotation.tailrec

type GameAction[A] = State[GameState, A]

val drawCard: GameAction[Card] = State { s =>
  val card :: rest = s.deck : @unchecked
  (s.copy(deck = rest), card)
}

val playerHit: GameAction[GamePhase] =
  for
    card <- drawCard
    _ <- State.modify[GameState] { s =>
      val h     = s.playerHand :+ card
      val phase = if isBust(h) then GamePhase.Resolved(Outcome.PlayerBusts) else GamePhase.PlayerTurn
      s.copy(playerHand = h, phase = phase)
    }
    phase <- State.inspect[GameState, GamePhase](_.phase)
  yield phase

val playerStand: GameAction[Outcome] = State { s =>
  @tailrec def dealerLoop(st: GameState): GameState =
    if dealerShouldHit(st.dealerHand) then
      val card :: rest = st.deck : @unchecked
      dealerLoop(st.copy(deck = rest, dealerHand = st.dealerHand :+ card))
    else st
  val finalSt = dealerLoop(s.copy(phase = GamePhase.DealerTurn))
  val outcome = resolveOutcome(finalSt.playerHand, finalSt.dealerHand)
  (finalSt.copy(phase = GamePhase.Resolved(outcome)), outcome)
}

def resolveOutcome(player: Hand, dealer: Hand): Outcome =
  (isBlackjack(player), isBlackjack(dealer), handValue(player), handValue(dealer)) match
    case (true, false, _, _) => Outcome.PlayerBlackjack
    case (true, true,  _, _) => Outcome.Push
    case (_, _, _, d) if d > 21 => Outcome.DealerBusts
    case (_, _, p, d) if p > d  => Outcome.PlayerWins
    case (_, _, p, d) if d > p  => Outcome.DealerWins
    case _                      => Outcome.Push
