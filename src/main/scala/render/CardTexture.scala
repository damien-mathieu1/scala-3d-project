package render

import model.*
import model.Card.*
import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js
import THREE.CanvasTexture

private val W = 256
private val H = 384

def makeCardTexture(card: Card): CanvasTexture =
  val canvas = document.createElement("canvas").asInstanceOf[dom.html.Canvas]
  canvas.width  = W
  canvas.height = H
  val ctx = canvas.getContext("2d").asInstanceOf[js.Dynamic]

  val isRed = card.suit == Suit.Hearts || card.suit == Suit.Diamonds
  val color  = if isRed then "#cc0000" else "#111111"
  val symbol = suitSymbol(card.suit)
  val label  = rankLabel(card.rank)

  // White background
  ctx.fillStyle = "white"
  ctx.fillRect(0, 0, W, H)

  // Border
  ctx.strokeStyle = "#aaaaaa"
  ctx.lineWidth = 3
  ctx.strokeRect(4, 4, W - 8, H - 8)

  // Top-left rank + suit
  ctx.fillStyle = color
  ctx.font = s"bold 48px Arial"
  ctx.textAlign = "left"
  ctx.textBaseline = "top"
  ctx.fillText(label, 12, 10)

  ctx.font = "36px Arial"
  ctx.fillText(symbol, 12, 60)

  // Centre large suit symbol
  ctx.font = s"128px Arial"
  ctx.textAlign = "center"
  ctx.textBaseline = "middle"
  ctx.fillText(symbol, W / 2, H / 2)

  // Bottom-right (rotated 180°)
  ctx.save()
  ctx.translate(W, H)
  ctx.rotate(Math.PI)
  ctx.font = s"bold 48px Arial"
  ctx.fillStyle = color
  ctx.textAlign = "left"
  ctx.textBaseline = "top"
  ctx.fillText(label, 12, 10)
  ctx.font = "36px Arial"
  ctx.fillText(symbol, 12, 60)
  ctx.restore()

  new CanvasTexture(canvas)

val backTexture: CanvasTexture =
  val canvas = document.createElement("canvas").asInstanceOf[dom.html.Canvas]
  canvas.width  = W
  canvas.height = H
  val ctx = canvas.getContext("2d").asInstanceOf[js.Dynamic]

  // Dark blue background
  ctx.fillStyle = "#1a3a6b"
  ctx.fillRect(0, 0, W, H)

  // Diagonal stripe pattern
  ctx.strokeStyle = "#2a5aab"
  ctx.lineWidth = 6
  var i = -H
  while i < W + H do
    ctx.beginPath()
    ctx.moveTo(i, 0)
    ctx.lineTo(i + H, H)
    ctx.stroke()
    i += 24

  // White border
  ctx.strokeStyle = "rgba(255,255,255,0.5)"
  ctx.lineWidth = 6
  ctx.strokeRect(12, 12, W - 24, H - 24)

  new CanvasTexture(canvas)
