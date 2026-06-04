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

val MAX_BET   = 400
val MAX_HANDS = 4   // cap on resplits

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

// ── Dealer + resolution (pure helpers) ───────────────────────────────────────

@tailrec
private def dealerLoop(hand: Hand, deck: List[Card]): (Hand, List[Card]) =
  if dealerShouldHit(hand) then
    val c :: rest = deck : @unchecked
    dealerLoop(hand :+ c, rest)
  else (hand, deck)

/** Resolve a single hand against the dealer's final hand.
 *  `isSplit` disables natural-blackjack scoring (a 21 made after a split is not a blackjack). */
private def resolveHand(h: PlayerHand, dealer: Hand, isSplit: Boolean): Outcome =
  if h.outcome.contains(Outcome.Surrender) then Outcome.Surrender
  else if isBust(h.cards) then Outcome.PlayerBusts
  else
    val pBJ = !isSplit && isBlackjack(h.cards)
    val dBJ = isBlackjack(dealer)
    (pBJ, dBJ, handValue(h.cards), handValue(dealer)) match
      case (true, false, _, _)    => Outcome.PlayerBlackjack
      case (true, true,  _, _)    => Outcome.Push
      case (false, true, _, _)    => Outcome.DealerWins
      case (_, _, _, d) if d > 21 => Outcome.DealerBusts
      case (_, _, p, d) if p > d  => Outcome.PlayerWins
      case (_, _, p, d) if d > p  => Outcome.DealerWins
      case _                      => Outcome.Push

/** Play the dealer out (only if a hand still needs comparison) and tag every hand
 *  with its outcome, moving the round to Resolved. */
private def resolveRound(s: GameState): GameState =
  val anyLive = s.hands.exists(h => !isBust(h.cards) && !h.outcome.contains(Outcome.Surrender))
  val (dh, deck) = if anyLive then dealerLoop(s.dealerHand, s.deck) else (s.dealerHand, s.deck)
  val isSplit    = s.hands.length > 1
  val resolved   = s.hands.map(h => h.copy(outcome = Some(resolveHand(h, dh, isSplit)), done = true))
  s.copy(deck = deck, dealerHand = dh, hands = resolved, phase = GamePhase.Resolved)

/** Move to the next unfinished hand, or resolve the round if all are done. */
private def advance(s: GameState): GameState =
  val next = s.hands.indexWhere(!_.done, s.active + 1)
  if next >= 0 then s.copy(active = next, phase = GamePhase.PlayerTurn)
  else resolveRound(s)

// ── Dealing ──────────────────────────────────────────────────────────────────

val dealInitialCards: GameAction[Unit] =
  for
    p1 <- drawCard
    d1 <- drawCard
    p2 <- drawCard
    d2 <- drawCard
    _  <- State.modify[GameState] { s =>
      val hand   = PlayerHand(List(p1, p2), s.bet)
      val dealer = List(d1, d2)
      val s1     = s.copy(hands = List(hand), active = 0, dealerHand = dealer)
      if d1.rank == Rank.Ace then s1.copy(phase = GamePhase.Insurance)       // offer insurance
      else if isBlackjack(hand.cards) then resolveRound(s1)                  // player natural → settle now
      else s1.copy(phase = GamePhase.PlayerTurn)
    }
  yield ()

// ── Insurance ────────────────────────────────────────────────────────────────

/** Continue after the insurance decision: a dealer peek for blackjack ends the
 *  round immediately, otherwise the player turn begins. */
private def afterInsurance(s: GameState): GameState =
  if isBlackjack(s.dealerHand) then resolveRound(s)
  else s.copy(phase = GamePhase.PlayerTurn)

val takeInsurance: GameAction[Unit] =
  State.modify { s =>
    val ins = s.bet / 2
    val s1  = if ins > 0 && s.balance >= ins then s.copy(balance = s.balance - ins, insurance = ins) else s
    afterInsurance(s1)
  }

val declineInsurance: GameAction[Unit] =
  State.modify(afterInsurance)

// ── Player actions (operate on the active hand) ──────────────────────────────

val playerHit: GameAction[GamePhase] =
  for
    card <- drawCard
    _ <- State.modify[GameState] { s =>
      val h      = s.hands(s.active)
      val nh     = h.copy(cards = h.cards :+ card)
      val busted = isBust(nh.cards)
      val s1     = s.copy(hands = s.hands.updated(s.active, nh.copy(done = busted)))
      if busted then advance(s1) else s1
    }
    phase <- State.inspect[GameState, GamePhase](_.phase)
  yield phase

val playerStand: GameAction[Unit] =
  State.modify { s =>
    advance(s.copy(hands = s.hands.updated(s.active, s.hands(s.active).copy(done = true))))
  }

val doubleDown: GameAction[Unit] =
  State.modify { s =>
    val h        = s.hands(s.active)
    val c :: rest = s.deck : @unchecked
    val nh       = h.copy(cards = h.cards :+ c, bet = h.bet * 2, done = true)
    advance(s.copy(balance = s.balance - h.bet, deck = rest, hands = s.hands.updated(s.active, nh)))
  }

/** Split the active pair into two hands, dealing one fresh card to each.
 *  Split aces receive a single card and are then finished. */
val split: GameAction[Unit] =
  State.modify { s =>
    val h = s.hands(s.active)
    val d1 :: d2 :: rest = s.deck : @unchecked
    val isAces = h.cards(0).rank == Rank.Ace
    val handA  = PlayerHand(List(h.cards(0), d1), h.bet, done = isAces)
    val handB  = PlayerHand(List(h.cards(1), d2), h.bet, done = isAces)
    val s1     = s.copy(
      balance = s.balance - h.bet,
      deck    = rest,
      hands   = s.hands.patch(s.active, List(handA, handB), 1)
    )
    if isAces then advance(s1) else s1.copy(phase = GamePhase.PlayerTurn)
  }

/** Late surrender: forfeit the (single, untouched) hand for half the bet back. */
val surrender: GameAction[Unit] =
  State.modify { s =>
    val nh = s.hands(s.active).copy(done = true, outcome = Some(Outcome.Surrender))
    advance(s.copy(hands = s.hands.updated(s.active, nh)))
  }

// ── Payout ───────────────────────────────────────────────────────────────────

val collectWinnings: GameAction[Int] = State { s =>
  val handPayout = s.hands.map { h =>
    h.outcome match
      case Some(Outcome.PlayerBlackjack) => h.bet + h.bet * 3 / 2
      case Some(Outcome.PlayerWins)      => h.bet * 2
      case Some(Outcome.DealerBusts)     => h.bet * 2
      case Some(Outcome.Push)            => h.bet
      case Some(Outcome.Surrender)       => h.bet / 2
      case _                             => 0
  }.sum
  val insPayout = if s.insurance > 0 && isBlackjack(s.dealerHand) then s.insurance * 3 else 0
  val payout    = handPayout + insPayout
  (s.copy(balance = s.balance + payout, bet = 0, insurance = 0), payout)
}
