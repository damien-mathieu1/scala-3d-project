package render

import THREE.Mesh
import org.scalajs.dom

// Global animation queue — each updater returns false when done
object Animator:
  private var updates: List[() => Boolean] = Nil
  def add(f: () => Boolean): Unit = updates = f :: updates
  def tick(): Unit = updates = updates.filter(_())

private def easeOut(t: Double): Double = 1 - Math.pow(1 - t.min(1.0), 3)
private def easeInOut(t: Double): Double =
  val c = t.max(0.0).min(1.0)
  if c < 0.5 then 2 * c * c else 1 - Math.pow(-2 * c + 2, 2) / 2
private def now(): Double = dom.window.performance.now()

// Card is drawn off the top of the shoe: it lifts out of the pile, arcs over,
// and settles flat at its place on the table.
def animateSlide(mesh: Mesh, targetX: Double, targetY: Double, startX: Double, startY: Double, delay: Double = 0): Unit =
  val duration = 500.0
  val targetZ  = mesh.position.z.asInstanceOf[Double]
  val startZ   = DECK_TOP_Z
  mesh.position.x = startX
  mesh.position.y = startY
  mesh.position.z = startZ
  val t0 = now()
  Animator.add(() =>
    val elapsed = now() - t0 - delay
    if elapsed < 0 then true // waiting for delay
    else
      val t = (elapsed / duration).min(1.0)
      val e = easeOut(t)
      mesh.position.x = startX + (targetX - startX) * e
      mesh.position.y = startY + (targetY - startY) * e
      // base z lerp + an upward hop so the card visibly peels off the pile
      mesh.position.z = startZ + (targetZ - startZ) * e + Math.sin(t * Math.PI) * 0.5
      t < 1.0
  )

// Reveal: the card eases through its half-turn while lifting and swelling
// slightly, then drops back flush.
def animateFlip(mesh: Mesh, onComplete: () => Unit = () => ()): Unit =
  val duration = 620.0
  val baseZ    = mesh.position.z.asInstanceOf[Double]
  val scaleDyn = mesh.scale.asInstanceOf[scala.scalajs.js.Dynamic]
  val t0 = now()
  Animator.add(() =>
    val t    = ((now() - t0) / duration).min(1.0)
    val hop  = Math.sin(t * Math.PI)
    mesh.rotation.y   = easeInOut(t) * Math.PI
    mesh.position.z   = baseZ + hop * 0.9
    val sc = 1.0 + hop * 0.14
    scaleDyn.set(sc, sc, sc)
    if t >= 1.0 then
      mesh.position.z = baseZ
      scaleDyn.set(1.0, 1.0, 1.0)
      onComplete()
    t < 1.0
  )

def animateChipSpread(chips: List[Mesh], spread: Boolean): Unit =
  val duration = 250.0
  val startXs: Array[Double] = chips.map(_.position.x.asInstanceOf[Double]).toArray
  val targetXs: Array[Double] = 
    if spread then
      val w = 0.6
      val tot = (chips.length - 1) * w
      (0 until chips.length).map(i => -tot / 2.0 + i * w).toArray
    else
      chips.indices.map(_ => 0.0).toArray
      
  val t0 = now()
  Animator.add(() =>
    val t = ((now() - t0) / duration).min(1.0)
    val e = easeOut(t)
    chips.zipWithIndex.foreach { case (m, i) =>
      m.position.x = startXs(i) + (targetXs(i) - startXs(i)) * e
    }
    t < 1.0
  )
