package effects

import model.*

val fullDeck: List[Card] =
  for
    s <- Suit.values.toList
    r <- Rank.values.toList
  yield Card(r, s)

val shuffledDeck: IO[List[Card]] = IO(scala.util.Random.shuffle(fullDeck))

def bettingState(balance: Int = 1000, minBet: Int = 1, maxBet: Int = 400): IO[GameState] =
  shuffledDeck.map(deck => GameState(deck, Nil, Nil, GamePhase.Betting, balance, 0, minBet, maxBet))
