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
  val startX   = targetX + 10.0
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
