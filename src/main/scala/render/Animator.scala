package render

import THREE.Mesh
import org.scalajs.dom

// Global animation queue — each updater returns false when done
object Animator:
  private var updates: List[() => Boolean] = Nil
  def add(f: () => Boolean): Unit = updates = f :: updates
  def tick(): Unit = updates = updates.filter(_())

private def easeOut(t: Double): Double = 1 - Math.pow(1 - t.min(1.0), 3)
private def now(): Double = dom.window.performance.now()

def animateSlide(mesh: Mesh, targetX: Double, delay: Double = 0): Unit =
  val duration = 450.0
  val startX   = targetX + 4.0
  mesh.position.x = startX
  val t0 = now()
  Animator.add(() =>
    val elapsed = now() - t0 - delay
    if elapsed < 0 then true // waiting for delay
    else
      val t = (elapsed / duration).min(1.0)
      mesh.position.x = startX + (targetX - startX) * easeOut(t)
      t < 1.0
  )

def animateFlip(mesh: Mesh, onComplete: () => Unit = () => ()): Unit =
  val duration = 480.0
  val t0 = now()
  Animator.add(() =>
    val t = ((now() - t0) / duration).min(1.0)
    mesh.rotation.y = t * Math.PI
    if t >= 1.0 then onComplete()
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
