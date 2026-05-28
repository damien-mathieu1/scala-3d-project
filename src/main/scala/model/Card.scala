package model

import cats.Show
import cats.kernel.Eq

enum Suit:
  case Hearts, Diamonds, Clubs, Spades

enum Rank:
  case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten
  case Jack, Queen, King, Ace

case class Card(rank: Rank, suit: Suit)

object Card:
  def suitSymbol(s: Suit): String = s match
    case Suit.Hearts   => "♥"
    case Suit.Diamonds => "♦"
    case Suit.Clubs    => "♣"
    case Suit.Spades   => "♠"

  def rankLabel(r: Rank): String = r match
    case Rank.Ace   => "A"
    case Rank.King  => "K"
    case Rank.Queen => "Q"
    case Rank.Jack  => "J"
    case Rank.Ten   => "10"
    case other      => (other.ordinal + 2).toString

  given Show[Suit] = s => suitSymbol(s)
  given Show[Rank] = r => rankLabel(r)
  given Show[Card] = c => s"${rankLabel(c.rank)}${suitSymbol(c.suit)}"
  given Eq[Card]   = Eq.fromUniversalEquals
