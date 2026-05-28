import modes.*
import render.{*, given}
import render.Animator
import THREE.*
import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js

@main def main(): Unit =

  // ── Scene setup (shared between modes) ───────────────────────────────────
  val container = document.getElementById("canvas-container")
  val w = dom.window.innerWidth
  val h = dom.window.innerHeight

  val renderer = WebGLRenderer(antialias = true)
  renderer.setSize(w, h)
  renderer.setPixelRatio(dom.window.devicePixelRatio)
  container.appendChild(renderer.domElement)

  val scene    = new Scene()
  val camera   = new PerspectiveCamera(55, w / h, 0.1, 100)
  val controls = new OrbitControls(camera, renderer.domElement)
  controls.enableDamping = true
  controls.dampingFactor = 0.05
  // js.Dynamic used because the facade doesn't expose `enabled`
  val controlsDyn = controls.asInstanceOf[js.Dynamic]

  def setControlsActive(active: Boolean): Unit =
    controlsDyn.enabled = active

  // ── Mode management ───────────────────────────────────────────────────────
  def showPanel(id: String): Unit =
    Seq("panel-menu", "panel-blackjack", "panel-viewer").foreach { pid =>
      val el = document.getElementById(pid)
      el.asInstanceOf[js.Dynamic].style.display = if pid == id then "block" else "none"
    }

  def activateBlackjack(): Unit =
    setControlsActive(false)
    showPanel("panel-blackjack")
    startBlackjack(scene, camera, renderer.domElement)

  def activateViewer(): Unit =
    setControlsActive(true)
    showPanel("panel-viewer")
    startImageViewer(scene, camera, controls)

  def activateMenu(): Unit =
    setControlsActive(false)
    showPanel("panel-menu")
    scene.clear()
    scene.background = new ThreeColor(0x0a0a1a).asInstanceOf[js.Any]
    camera.position.set(0, 0, 5)
    camera.lookAt(0, 0, 0)

  // ── Menu buttons ──────────────────────────────────────────────────────────
  document.getElementById("menu-btn-blackjack").addEventListener("click", (_: dom.Event) =>
    activateBlackjack()
  )
  document.getElementById("menu-btn-viewer").addEventListener("click", (_: dom.Event) =>
    activateViewer()
  )
  Seq("back-from-blackjack", "back-from-viewer").foreach { id =>
    document.getElementById(id).addEventListener("click", (_: dom.Event) => activateMenu())
  }

  // ── Resize ────────────────────────────────────────────────────────────────
  dom.window.addEventListener("resize", (_: dom.Event) =>
    val w2 = dom.window.innerWidth
    val h2 = dom.window.innerHeight
    camera.aspect = w2 / h2
    camera.updateProjectionMatrix()
    renderer.setSize(w2, h2)
  )

  // ── Render loop ───────────────────────────────────────────────────────────
  renderer.setAnimationLoop(() =>
    Animator.tick()
    if controlsDyn.enabled.asInstanceOf[Boolean] then controls.update()
    renderer.render(scene, camera)
  )

  // ── Start on menu ─────────────────────────────────────────────────────────
  activateMenu()
