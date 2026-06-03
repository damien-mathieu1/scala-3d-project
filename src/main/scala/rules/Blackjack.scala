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

val MAX_BET = 400

def placeBet(amount: Int): GameAction[Unit] =
  State.modify { s =>
    if amount > 0 && amount <= s.balance && s.bet + amount <= MAX_BET
    then s.copy(balance = s.balance - amount, bet = s.bet + amount)
    else s
  }

val clearBet: GameAction[Unit] =
  State.modify { s =>
    s.copy(balance = s.balance + s.bet, bet = 0)
  }

val dealInitialCards: GameAction[Unit] =
  for
    p1 <- drawCard
    d1 <- drawCard
    p2 <- drawCard
    d2 <- drawCard
    _  <- State.modify[GameState](s => s.copy(
            playerHand = List(p1, p2),
            dealerHand = List(d1, d2),
            phase      = GamePhase.PlayerTurn
          ))
  yield ()

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

val doubleDown: GameAction[Outcome] = State { s =>
  val s1           = s.copy(balance = s.balance - s.bet, bet = s.bet * 2)
  val card :: rest = s1.deck : @unchecked
  val newHand      = s1.playerHand :+ card
  val s2           = s1.copy(deck = rest, playerHand = newHand)
  if isBust(newHand) then
    (s2.copy(phase = GamePhase.Resolved(Outcome.PlayerBusts)), Outcome.PlayerBusts)
  else
    playerStand.run(s2).value
}

val collectWinnings: GameAction[Int] = State { s =>
  val payout = s.phase match
    case GamePhase.Resolved(Outcome.PlayerBlackjack) => s.bet + (s.bet * 3 / 2)
    case GamePhase.Resolved(Outcome.PlayerWins)      => s.bet * 2
    case GamePhase.Resolved(Outcome.DealerBusts)     => s.bet * 2
    case GamePhase.Resolved(Outcome.Push)            => s.bet
    case _                                           => 0
  (s.copy(balance = s.balance + payout, bet = 0), payout)
}

def resolveOutcome(player: Hand, dealer: Hand): Outcome =
  (isBlackjack(player), isBlackjack(dealer), handValue(player), handValue(dealer)) match
    case (true, false, _, _)             => Outcome.PlayerBlackjack
    case (true, true,  _, _)             => Outcome.Push
    case (_, _, _, d) if d > 21          => Outcome.DealerBusts
    case (_, _, p, d) if p > d           => Outcome.PlayerWins
    case (_, _, p, d) if d > p           => Outcome.DealerWins
    case _                               => Outcome.Push
