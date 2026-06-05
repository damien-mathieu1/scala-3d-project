package effects

import model.*

val fullDeck: List[Card] =
  for
    s <- Suit.values.toList
    r <- Rank.values.toList
  yield Card(r, s)

// Classic casino blackjack is dealt from a 6-deck shoe (312 cards).
val SHOE_DECKS = 6
val fullShoe: List[Card] = List.fill(SHOE_DECKS)(fullDeck).flatten

val shuffledDeck: IO[List[Card]] = IO(scala.util.Random.shuffle(fullShoe))

def bettingState(balance: Int = 1000, minBet: Int = 1, maxBet: Int = 400): IO[GameState] =
  shuffledDeck.map(deck => GameState(deck, Nil, 0, Nil, GamePhase.Betting, balance, 0, 0, minBet, maxBet))
