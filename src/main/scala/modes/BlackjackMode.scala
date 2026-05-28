package modes

import model.*
import model.Card.*
import model.Hand.*
import effects.*
import rules.*
import render.*

import THREE.*
import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

private val PLAYER_ROW = -1.5
private val DEALER_ROW =  1.5
private val ROW_GAP    =  1.2

def startBlackjack(scene: Scene, camera: PerspectiveCamera, canvas: dom.html.Canvas): Unit =
  scene.clear()
  scene.background = new ThreeColor(0x0d1f0d).asInstanceOf[js.Any]

  camera.position.set(0, 0, 7)
  camera.lookAt(0, 0, 0)
  camera.aspect = dom.window.innerWidth.toDouble / dom.window.innerHeight.toDouble
  camera.updateProjectionMatrix()

  var state: GameState       = initialState.unsafeRun()
  var cardMeshes: List[Mesh] = Nil
  var holeCard: js.UndefOr[Mesh] = js.undefined  // dealer's face-down card

  // ── Layout helpers ───────────────────────────────────────────────────────
  def rowX(n: Int, i: Int): Double = -(n - 1) * ROW_GAP / 2 + i * ROW_GAP

  // ── Render helpers ────────────────────────────────────────────────────────
  def addCard(mesh: Mesh, x: Double, y: Double, slideDelay: Double = 0): Unit =
    mesh.position.set(x, y, 0)
    scene.add(mesh)
    cardMeshes = cardMeshes :+ mesh
    animateSlide(mesh, x, slideDelay)

  def renderInitialDeal(): Unit =
    cardMeshes.foreach(m => scene.remove(m))
    cardMeshes = Nil
    holeCard = js.undefined

    val ph = state.playerHand
    val dh = state.dealerHand

    // Player cards (staggered)
    ph.zipWithIndex.foreach { (card, i) =>
      addCard(makeCardMesh(card), rowX(ph.length, i), PLAYER_ROW, i * 150.0)
    }
    // Dealer: first card face-up, second face-down (hole)
    val d0 = makeCardMesh(dh(0))
    addCard(d0, rowX(2, 0), DEALER_ROW, ph.length * 150.0)

    val hc = makeHoleCardMesh(dh(1))
    holeCard = hc
    addCard(hc, rowX(2, 1), DEALER_ROW, (ph.length + 1) * 150.0)

  def renderHit(card: Card): Unit =
    val n = state.playerHand.length  // already includes the new card
    // reposition all existing player cards
    val (playerMeshes, dealerMeshes) = cardMeshes.partition { m =>
      m.position.y.asInstanceOf[Double] < 0
    }
    playerMeshes.zipWithIndex.foreach { (m, i) =>
      m.position.set(rowX(n, i), PLAYER_ROW, m.position.z.asInstanceOf[Double])
    }
    // add new card (last in hand)
    val mesh = makeCardMesh(card)
    mesh.position.set(rowX(n, n - 1), PLAYER_ROW, 0)
    scene.add(mesh)
    cardMeshes = cardMeshes :+ mesh
    animateSlide(mesh, rowX(n, n - 1))

  def renderDealerReveal(): Unit =
    // Remove all old meshes, rebuild dealer hand fully face-up with slide
    cardMeshes.foreach(m => scene.remove(m))
    cardMeshes = Nil
    holeCard = js.undefined

    val ph = state.playerHand
    val dh = state.dealerHand

    ph.zipWithIndex.foreach { (card, i) =>
      val mesh = makeCardMesh(card)
      mesh.position.set(rowX(ph.length, i), PLAYER_ROW, 0)
      scene.add(mesh)
      cardMeshes = cardMeshes :+ mesh
    }
    dh.zipWithIndex.foreach { (card, i) =>
      val mesh = makeCardMesh(card)
      mesh.position.set(rowX(dh.length, i), DEALER_ROW, 0)
      scene.add(mesh)
      cardMeshes = cardMeshes :+ mesh
      if i >= 2 then animateSlide(mesh, rowX(dh.length, i), delay = (i - 2) * 200.0)
    }

  // ── UI ───────────────────────────────────────────────────────────────────
  def setButtons(hitEnabled: Boolean, standEnabled: Boolean): Unit =
    document.getElementById("btn-hit").asInstanceOf[dom.html.Button].disabled   = !hitEnabled
    document.getElementById("btn-stand").asInstanceOf[dom.html.Button].disabled = !standEnabled

  def updateUI(): Unit =
    val pScore = handValue(state.playerHand)
    val dScore = handValue(state.dealerHand)
    document.getElementById("player-score").textContent = s"Joueur: $pScore"
    document.getElementById("dealer-score").textContent = s"Dealer: $dScore"
    val (h, s_) = state.phase match
      case GamePhase.PlayerTurn => (true, true)
      case _                    => (false, false)
    setButtons(h, s_)
    val msg = state.phase match
      case GamePhase.Resolved(Outcome.PlayerWins)      => "🎉 Vous gagnez !"
      case GamePhase.Resolved(Outcome.PlayerBlackjack) => "🃏 Blackjack !"
      case GamePhase.Resolved(Outcome.DealerWins)      => "😞 Dealer gagne"
      case GamePhase.Resolved(Outcome.PlayerBusts)     => "💥 Bust !"
      case GamePhase.Resolved(Outcome.DealerBusts)     => "🎉 Dealer bust !"
      case GamePhase.Resolved(Outcome.Push)            => "🤝 Égalité"
      case _                                           => ""
    document.getElementById("result-msg").textContent = msg

  renderInitialDeal()
  updateUI()

  // ── Per-card drag rotation ────────────────────────────────────────────────
  val raycaster = new Raycaster()
  val mouse     = new Vector2()
  var dragMesh: js.UndefOr[Mesh] = js.undefined
  var lastX = 0.0; var lastY = 0.0

  canvas.addEventListener("mousedown", (e: dom.MouseEvent) =>
    mouse.x =  (e.clientX.toDouble / dom.window.innerWidth)  * 2 - 1
    mouse.y = -(e.clientY.toDouble / dom.window.innerHeight) * 2 + 1
    raycaster.setFromCamera(mouse, camera)
    val hits = raycaster.intersectObjects(cardMeshes.toJSArray)
    if hits.length > 0 then
      dragMesh = hits(0).`object`.asInstanceOf[Mesh]
      lastX = e.clientX; lastY = e.clientY
  )
  canvas.addEventListener("mousemove", (e: dom.MouseEvent) =>
    dragMesh.foreach { m =>
      m.rotation.y += (e.clientX - lastX) * 0.012
      m.rotation.x += (e.clientY - lastY) * 0.012
      lastX = e.clientX; lastY = e.clientY
    }
  )
  canvas.addEventListener("mouseup", (_: dom.Event) => dragMesh = js.undefined)

  // ── Game buttons ─────────────────────────────────────────────────────────
  document.getElementById("btn-hit").addEventListener("click", (_: dom.Event) =>
    state.phase match
      case GamePhase.PlayerTurn =>
        val prevLen = state.playerHand.length
        val (newState, _) = playerHit.run(state).value
        state = newState
        val newCard = state.playerHand.last
        renderHit(newCard)
        updateUI()
      case _ => ()
  )

  document.getElementById("btn-stand").addEventListener("click", (_: dom.Event) =>
    state.phase match
      case GamePhase.PlayerTurn =>
        setButtons(false, false)
        holeCard match
          case hc if !js.isUndefined(hc) =>
            animateFlip(hc.asInstanceOf[Mesh], () =>
              val (newState, _) = playerStand.run(state).value
              state = newState
              renderDealerReveal()
              updateUI()
            )
          case _ =>
            val (newState, _) = playerStand.run(state).value
            state = newState
            renderDealerReveal()
            updateUI()
      case _ => ()
  )

  document.getElementById("btn-new").addEventListener("click", (_: dom.Event) =>
    state = initialState.unsafeRun()
    renderInitialDeal()
    updateUI()
  )
