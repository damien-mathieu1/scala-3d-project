package render

import THREE.*
import scala.scalajs.js
import model.Card

val CARD_W = 1.05
val CARD_H = 1.47  // standard 2.5:3.5 ratio
val CARD_D = 0.008

val DECK_VISUAL_D = 0.6        // rendered thickness of the shoe
val DECK_TOP_Z    = DECK_VISUAL_D  // cards are drawn off the top of the pile

def makeCardMesh(card: Card): Mesh =
  val geo     = new BoxGeometry(CARD_W, CARD_H, CARD_D)
  val sideMat = MeshBasicMaterial(0xf0f0f0)
  val faceMat = new MeshBasicMaterial()
  faceMat.map = makeCardTexture(card)
  val backMat = new MeshBasicMaterial()
  backMat.map = backTexture
  // BoxGeometry face order: +X, -X, +Y, -Y, +Z (front/face), -Z (back)
  new Mesh(geo, js.Array[Material](sideMat, sideMat, sideMat, sideMat, faceMat, backMat))

// Hole card: +Z shows back (face-down), -Z shows face (revealed after Y-rotation of PI)
def makeHoleCardMesh(card: Card): Mesh =
  val geo     = new BoxGeometry(CARD_W, CARD_H, CARD_D)
  val sideMat = MeshBasicMaterial(0xf0f0f0)
  val faceMat = new MeshBasicMaterial()
  faceMat.map = makeCardTexture(card)
  val backMat = new MeshBasicMaterial()
  backMat.map = backTexture
  // Swap face/back vs normal: front (+Z) = back texture (hidden), back (-Z) = face (revealed)
  new Mesh(geo, js.Array[Material](sideMat, sideMat, sideMat, sideMat, backMat, faceMat))

// A thick block standing in for the whole shoe: card back on top/bottom,
// stacked-paper edges on the four sides.
def makeDeckMesh(): Mesh =
  val geo  = new BoxGeometry(CARD_W, CARD_H, DECK_VISUAL_D)
  val edge = new MeshBasicMaterial()
  edge.map = deckEdgeTexture
  val back = new MeshBasicMaterial()
  back.map = backTexture
  // face order: +X, -X, +Y, -Y, +Z (top of pile), -Z (bottom)
  new Mesh(geo, js.Array[Material](edge, edge, edge, edge, back, back))
