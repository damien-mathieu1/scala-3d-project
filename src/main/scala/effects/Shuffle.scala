package effects

import model.*

val fullDeck: List[Card] =
  for
    s <- Suit.values.toList
    r <- Rank.values.toList
  yield Card(r, s)

val shuffledDeck: IO[List[Card]] = IO(scala.util.Random.shuffle(fullDeck))

def initialState: IO[GameState] = shuffledDeck.map { deck =>
  val p1 :: d1 :: p2 :: d2 :: rest = deck : @unchecked
  GameState(rest, List(p1, p2), List(d1, d2), GamePhase.PlayerTurn)
}
