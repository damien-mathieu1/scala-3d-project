package modes

import render.*
import THREE.*
import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native @JSImport("three", "PlaneGeometry")
private class ViewerPlaneGeometry(w: Double = 1, h: Double = 1) extends BufferGeometry

@js.native @JSImport("three", "SphereGeometry")
private class ViewerSphereGeometry(r: Double = 1, ws: Int = 64, hs: Int = 32) extends BufferGeometry

def startImageViewer(
  scene: Scene,
  camera: PerspectiveCamera,
  controls: OrbitControls
): Unit =
  scene.clear()
  scene.background = new ThreeColor(0x1a1a2e).asInstanceOf[js.Any]

  camera.position.set(0, 0, 3)
  camera.lookAt(0, 0, 0)
  camera.aspect = dom.window.innerWidth.toDouble / dom.window.innerHeight.toDouble
  camera.updateProjectionMatrix()

  controls.target.set(0, 0, 0)
  controls.update()

  var currentMesh: js.UndefOr[Mesh]    = js.undefined
  var currentTexture: js.UndefOr[Texture] = js.undefined
  var currentShape: String             = "plane"

  def buildGeometry(shape: String, aspect: Double): BufferGeometry =
    shape match
      case "box"    => new BoxGeometry(aspect, 1.0, aspect * 0.04)
      case "sphere" => new ViewerSphereGeometry(1.2)
      case _        => new ViewerPlaneGeometry(aspect, 1.0)

  def updateScene(shape: String): Unit =
    currentTexture.foreach { tex =>
      currentMesh.foreach(m => scene.remove(m))
      val imgEl  = tex.image.asInstanceOf[js.Dynamic]
      val aspect = imgEl.width.asInstanceOf[Double] / imgEl.height.asInstanceOf[Double]
      val geo    = buildGeometry(shape, aspect)
      val mat    = new MeshBasicMaterial()
      mat.map    = tex
      val mesh   = new Mesh(geo, mat)
      scene.add(mesh)
      currentMesh = mesh
    }

  val fileInput = document.getElementById("viewer-file").asInstanceOf[dom.html.Input]
  fileInput.addEventListener("change", (_: dom.Event) =>
    val files = fileInput.files
    if files.length > 0 then
      val url    = dom.URL.createObjectURL(files(0))
      val loader = new TextureLoader()
      loader.load(url, (tex: Texture) =>
        currentTexture = tex
        updateScene(currentShape)
      )
  )

  def setShape(shape: String): Unit =
    currentShape = shape
    Seq("plane", "box", "sphere").foreach { s =>
      val btn = document.getElementById(s"viewer-btn-$s")
      if s == shape then btn.classList.add("active")
      else btn.classList.remove("active")
    }
    updateScene(shape)

  document.getElementById("viewer-btn-plane").addEventListener("click",  (_: dom.Event) => setShape("plane"))
  document.getElementById("viewer-btn-box").addEventListener("click",    (_: dom.Event) => setShape("box"))
  document.getElementById("viewer-btn-sphere").addEventListener("click", (_: dom.Event) => setShape("sphere"))
