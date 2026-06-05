package render

import THREE.*
import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js
import scala.math.Pi

val CHIP_RADIUS    = 0.26
val CHIP_THICKNESS = 0.10
private val CHIP_SEGMENTS = 64

private def chipHex(denomination: Int): Int = denomination match
  case 1   => 0xe8e8e8
  case 2   => 0xc0a0d0
  case 5   => 0xcc2222
  case 25  => 0x22aa44
  case 50  => 0x2255cc
  case 100 => 0x1a1a1a
  case 200 => 0xd4701a
  case 500 => 0x8800cc
  case _   => 0x888888

private def cssHex(hex: Int): String = f"#$hex%06x"

def makeChipCanvas(denomination: Int): dom.html.Canvas =
  val hex = chipHex(denomination)
  makeChipCanvasForHex(denomination, hex)

private def makeChipCanvasForHex(denomination: Int, hex: Int): dom.html.Canvas =
  val S   = 256
  val cx  = S / 2.0; val cy = S / 2.0
  val r   = S / 2.0 - 4.0
  val col = cssHex(hex)
  val acc = if hex == 0x1a1a1a then "#d4af37" else "white"

  val canvas = document.createElement("canvas").asInstanceOf[dom.html.Canvas]
  canvas.width = S; canvas.height = S
  val ctx = canvas.getContext("2d").asInstanceOf[js.Dynamic]

  ctx.beginPath()
  ctx.arc(cx, cy, r, 0, 2 * Math.PI)
  ctx.fillStyle = col
  ctx.fill()

  val g1 = ctx.createRadialGradient(cx - r * 0.32, cy - r * 0.32, r * 0.04, cx, cy, r)
  g1.addColorStop(0, "rgba(255,255,255,0.30)")
  g1.addColorStop(1, "rgba(0,0,0,0.28)")
  ctx.beginPath()
  ctx.arc(cx, cy, r, 0, 2 * Math.PI)
  ctx.fillStyle = g1
  ctx.fill()

  for i <- 0 until 8 do
    val a1 = i * 2 * Math.PI / 8 - Math.PI / 2
    val a2 = a1 + Math.PI / 8
    ctx.beginPath()
    ctx.arc(cx, cy, r,        a1, a2)
    ctx.arc(cx, cy, r * 0.67, a2, a1, true)
    ctx.closePath()
    ctx.fillStyle = acc
    ctx.fill()

  ctx.beginPath()
  ctx.arc(cx, cy, r * 0.67, 0, 2 * Math.PI)
  ctx.fillStyle = col
  ctx.fill()

  val g2 = ctx.createRadialGradient(cx - r * 0.2, cy - r * 0.2, 0, cx, cy, r * 0.67)
  g2.addColorStop(0, "rgba(255,255,255,0.20)")
  g2.addColorStop(1, "rgba(0,0,0,0.22)")
  ctx.beginPath()
  ctx.arc(cx, cy, r * 0.67, 0, 2 * Math.PI)
  ctx.fillStyle = g2
  ctx.fill()

  ctx.beginPath()
  ctx.arc(cx, cy, r * 0.67, 0, 2 * Math.PI)
  ctx.strokeStyle = acc
  ctx.lineWidth   = 1.5
  ctx.stroke()

  val (centerC0, centerC1, textCol, shadowCol) =
    if hex == 0x1a1a1a then
      ("white", "#e8e8e8", "#d4af37", "rgba(180,120,0,0.6)")
    else
      (acc, "#c8c8c8", col, "rgba(0,0,0,0.45)")
  val g3 = ctx.createRadialGradient(cx - r * 0.1, cy - r * 0.1, 0, cx, cy, r * 0.38)
  g3.addColorStop(0, centerC0)
  g3.addColorStop(1, centerC1)
  ctx.beginPath()
  ctx.arc(cx, cy, r * 0.38, 0, 2 * Math.PI)
  ctx.fillStyle = g3
  ctx.fill()

  val fs = if denomination >= 100 then 50 else 64
  ctx.font         = s"bold ${fs}px Arial"
  ctx.textAlign    = "center"
  ctx.textBaseline = "middle"
  ctx.shadowColor  = shadowCol
  ctx.shadowBlur   = 4.0
  ctx.fillStyle    = textCol
  ctx.fillText(denomination.toString, cx, cy)
  ctx.shadowBlur   = 0.0

  ctx.beginPath()
  ctx.arc(cx, cy, r - 1.5, 0, 2 * Math.PI)
  ctx.strokeStyle = "rgba(255,255,255,0.72)"
  ctx.lineWidth   = 3.0
  ctx.stroke()

  val shine = ctx.createRadialGradient(cx - r * 0.20, cy - r * 0.26, 0, cx, cy, r)
  shine.addColorStop(0, "rgba(255,255,255,0.40)")
  shine.addColorStop(0.42, "rgba(255,255,255,0.07)")
  shine.addColorStop(1, "rgba(255,255,255,0.00)")
  ctx.beginPath()
  ctx.arc(cx, cy, r - 2, 0, 2 * Math.PI)
  ctx.fillStyle = shine
  ctx.fill()

  canvas

private def makeChipTexture(denomination: Int, hex: Int): CanvasTexture =
  new CanvasTexture(makeChipCanvasForHex(denomination, hex))

def makeChipMesh(denomination: Int): Mesh =
  val hex     = chipHex(denomination)
  val geo     = new CylinderGeometry(CHIP_RADIUS, CHIP_RADIUS, CHIP_THICKNESS, CHIP_SEGMENTS)
  val capMat  = new MeshBasicMaterial()
  capMat.map  = makeChipTexture(denomination, hex)
  val sideHex = if hex == 0x1a1a1a then 0xd4af37 else hex
  val sideMat = MeshBasicMaterial(sideHex)
  new Mesh(geo, js.Array[Material](sideMat, capMat, capMat))
