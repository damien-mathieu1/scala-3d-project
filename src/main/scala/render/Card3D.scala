package render

import THREE.*
import scala.scalajs.js
import model.Card

val CARD_W = 1.05
val CARD_H = 1.47  // standard 2.5:3.5 ratio
val CARD_D = 0.008

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
