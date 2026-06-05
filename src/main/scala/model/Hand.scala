package model

type Hand = List[Card]

object Hand:
  def cardValue(rank: Rank): Int = rank match
    case Rank.Ace                           => 11
    case Rank.King | Rank.Queen | Rank.Jack => 10
    case other                              => other.ordinal + 2 // Two=0→2 ... Ten=8→10

  def handValue(hand: Hand): Int =
    val raw  = hand.foldLeft(0)((acc, c) => acc + cardValue(c.rank))
    val aces = hand.count(_.rank == Rank.Ace)
    // each excess Ace counted as 1 instead of 11
    (0 until aces).foldLeft(raw)((v, _) => if v > 21 then v - 10 else v)

  def isBust(hand: Hand): Boolean          = handValue(hand) > 21
  def isBlackjack(hand: Hand): Boolean     = hand.length == 2 && handValue(hand) == 21
  def dealerShouldHit(hand: Hand): Boolean = handValue(hand) < 17

  // two starting cards of equal value (10/J/Q/K all count as 10)
  def canSplit(hand: Hand): Boolean =
    hand.length == 2 && cardValue(hand(0).rank) == cardValue(hand(1).rank)
