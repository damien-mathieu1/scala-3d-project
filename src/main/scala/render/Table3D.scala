package render

import THREE.*
import org.scalajs.dom
import scala.scalajs.js
import scala.math.Pi

// Card placement constants
val PLAYER_Z  =  1.8
val DEALER_Z  = -1.8
val CARD_GAP  =  0.85
val CARD_Y    =  0.005  // just above table

def setupScene(container: dom.Element): (WebGLRenderer, Scene, PerspectiveCamera) =
  val w = dom.window.innerWidth
  val h = dom.window.innerHeight

  val renderer = WebGLRenderer(antialias = true)
  renderer.setSize(w, h)
  renderer.setPixelRatio(dom.window.devicePixelRatio)
  container.appendChild(renderer.domElement)

  val scene = new Scene()
  scene.background = new ThreeColor(0x0d2b0d).asInstanceOf[js.Any]

  val camera = new PerspectiveCamera(55, w / h, 0.1, 50)
  camera.position.set(0, 5.5, 4.5)
  camera.lookAt(0, 0, 0.5)

  // Table felt
  val tableMat = MeshBasicMaterial(0x1a5c1a)
  val tableGeo = new PlaneGeometry(8, 6)
  val table    = new Mesh(tableGeo, tableMat)
  table.rotation.x = -Pi / 2
  scene.add(table)

  // Faint zone lines
  val lineMat = MeshBasicMaterial(0x2a7a2a)
  val lineGeo = new PlaneGeometry(6, 0.02)
  val line    = new Mesh(lineGeo, lineMat)
  line.rotation.x = -Pi / 2
  line.position.y = 0.001
  scene.add(line)

  // Lighting (subtle, MeshBasicMaterial ignores it but useful if switching to Standard)
  val ambient = new AmbientLight(0xffffff, 0.8)
  scene.add(ambient)
  val dirLight = new DirectionalLight(0xffffff, 0.5)
  dirLight.position.set(2, 5, 3)
  scene.add(dirLight)

  (renderer, scene, camera)

def layoutHand(hand: model.Hand, z: Double): List[(model.Card, (Double, Double, Double))] =
  val n      = hand.length
  val startX = -(n - 1) * CARD_GAP / 2
  hand.zipWithIndex.map { (card, i) =>
    card -> (startX + i * CARD_GAP, CARD_Y, z)
  }
