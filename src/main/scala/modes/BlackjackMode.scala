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
import scala.math.Pi

private val PLAYER_ROW = -1.2
private val DEALER_ROW =  1.2
private val ROW_GAP    =  1.2
private val CHIP_ROW   = -2.5

private case class RenderState(
  game:     GameState,
  cards:    List[Mesh]       = Nil,
  chips:    List[Mesh]       = Nil,
  holeCard: js.UndefOr[Mesh] = js.undefined,
  rotX:     Double           = 0.0,
  rotY:     Double           = 0.0,
  velX:     Double           = 0.0,
  velY:     Double           = 0.0
)

def startBlackjack(scene: Scene, camera: PerspectiveCamera, canvas: dom.html.Canvas): Unit =
  scene.clear()
  scene.background = new ThreeColor(0x0d1f0d).asInstanceOf[js.Any]
  camera.position.set(0, 0, 7)
  camera.lookAt(0, 0, 0)
  camera.aspect = dom.window.innerWidth.toDouble / dom.window.innerHeight.toDouble
  camera.updateProjectionMatrix()

  var rs: RenderState = RenderState(bettingState().unsafeRun())
  def update(f: RenderState => RenderState): Unit = rs = f(rs)

  // Drag state — boundary vars for mouse interaction only
  var dragMesh:  js.UndefOr[Mesh] = js.undefined
  var dragChips: Boolean           = false
  var lastX = 0.0; var lastY = 0.0

  def rowX(n: Int, i: Int): Double = -(n - 1) * ROW_GAP / 2 + i * ROW_GAP

  // Pure scene transitions: RenderState => RenderState
  // Three.js mutations are side effects at the boundary

  def addCardToScene(mesh: Mesh, x: Double, y: Double, delay: Double = 0): RenderState => RenderState = s =>
    mesh.position.set(x, y, 0)
    scene.add(mesh)
    animateSlide(mesh, x, delay)
    s.copy(cards = s.cards :+ mesh)

  def deal: RenderState => RenderState = s =>
    s.cards.foreach(m => scene.remove(m))
    val ph  = s.game.playerHand
    val dh  = s.game.dealerHand
    val s1  = ph.zipWithIndex.foldLeft(s.copy(cards = Nil, holeCard = js.undefined)) {
      case (acc, (card, i)) =>
        addCardToScene(makeCardMesh(card), rowX(ph.length, i), PLAYER_ROW, i * 150.0)(acc)
    }
    val s2  = addCardToScene(makeCardMesh(dh(0)), rowX(2, 0), DEALER_ROW, ph.length * 150.0)(s1)
    val hc  = makeHoleCardMesh(dh(1))
    scene.add(hc)
    hc.position.set(rowX(2, 1), DEALER_ROW, 0)
    animateSlide(hc, rowX(2, 1), (ph.length + 1) * 150.0)
    s2.copy(cards = s2.cards :+ hc, holeCard = hc)

  def hit(card: Card): RenderState => RenderState = s =>
    val n = s.game.playerHand.length
    val (playerCards, _) = s.cards.partition(_.position.y.asInstanceOf[Double] < 0)
    playerCards.zipWithIndex.foreach { (m, i) =>
      m.position.set(rowX(n, i), PLAYER_ROW, m.position.z.asInstanceOf[Double])
    }
    val mesh = makeCardMesh(card)
    mesh.position.set(rowX(n, n - 1), PLAYER_ROW, 0)
    scene.add(mesh)
    animateSlide(mesh, rowX(n, n - 1))
    s.copy(cards = s.cards :+ mesh)

  def dealerReveal: RenderState => RenderState = s =>
    s.cards.foreach(m => scene.remove(m))
    val ph  = s.game.playerHand
    val dh  = s.game.dealerHand
    val s1  = ph.zipWithIndex.foldLeft(s.copy(cards = Nil, holeCard = js.undefined)) {
      case (acc, (card, i)) =>
        val mesh = makeCardMesh(card)
        mesh.position.set(rowX(ph.length, i), PLAYER_ROW, 0)
        scene.add(mesh)
        acc.copy(cards = acc.cards :+ mesh)
    }
    dh.zipWithIndex.foldLeft(s1) {
      case (acc, (card, i)) =>
        val mesh = makeCardMesh(card)
        mesh.position.set(rowX(dh.length, i), DEALER_ROW, 0)
        scene.add(mesh)
        if i >= 2 then animateSlide(mesh, rowX(dh.length, i), (i - 2) * 200.0)
        acc.copy(cards = acc.cards :+ mesh)
    }

  def addChip(denomination: Int): RenderState => RenderState = s =>
    val n     = s.chips.length + 1
    val scale = (1.0 / math.sqrt(n.toDouble)).max(0.35)
    val z     = s.chips.length.toDouble * 0.015
    val mesh  = makeChipMesh(denomination)
    mesh.position.set(0.0, CHIP_ROW, z)
    mesh.rotation.asInstanceOf[js.Dynamic].x = Pi / 2 + s.rotX
    mesh.rotation.asInstanceOf[js.Dynamic].y = s.rotY
    mesh.scale.set(scale, scale, scale)
    s.chips.foreach(_.scale.set(scale, scale, scale))
    scene.add(mesh)
    s.copy(chips = s.chips :+ mesh)

  def clearChips: RenderState => RenderState = s =>
    s.chips.foreach(m => scene.remove(m))
    s.copy(chips = Nil, rotX = 0.0, rotY = 0.0, velX = 0.0, velY = 0.0)

  def addChipsForAmount(amount: Int): RenderState => RenderState = startState =>
    List(100, 50, 25, 5).foldLeft((amount, startState)) { case ((rem, acc), d) =>
      val newAcc = (1 to (rem / d)).foldLeft(acc)((a, _) => addChip(d)(a))
      (rem % d, newAcc)
    }._2

  def newRound: RenderState => RenderState = s =>
    val cleaned = clearChips(s)
    cleaned.cards.foreach(m => scene.remove(m))
    cleaned.copy(cards = Nil, holeCard = js.undefined, game = bettingState(s.game.balance).unsafeRun())

  // UI helpers

  def el(id: String)  = document.getElementById(id).asInstanceOf[dom.html.Element]
  def btn(id: String) = document.getElementById(id).asInstanceOf[dom.html.Button]

  def setDisplay(id: String, d: String): Unit = el(id).style.display = d

  def setButtons(h: Boolean, s: Boolean, d: Boolean = false): Unit =
    btn("btn-hit").disabled    = !h
    btn("btn-stand").disabled  = !s
    btn("btn-double").disabled = !d

  def showBettingPhase(): Unit =
    setDisplay("betting-controls", "flex")
    setDisplay("controls-bj",      "none")
    setDisplay("label-dealer",     "none")
    setDisplay("label-player",     "none")
    el("dealer-score").textContent = ""
    el("player-score").textContent = ""
    el("result-msg").textContent   = ""

  def showGamePhase(): Unit =
    setDisplay("betting-controls", "none")
    setDisplay("controls-bj",      "flex")
    setDisplay("label-dealer",     "block")
    setDisplay("label-player",     "block")

  def updateScores(): Unit =
    el("balance-display").textContent = s"${rs.game.balance}€"
    el("bet-display").textContent     = s"Mise: ${rs.game.bet}€"
    rs.game.phase match
      case GamePhase.Betting    => ()
      case GamePhase.PlayerTurn =>
        el("player-score").textContent = s"Joueur: ${handValue(rs.game.playerHand)}"
        el("dealer-score").textContent = "Dealer: ?"
      case _ =>
        el("player-score").textContent = s"Joueur: ${handValue(rs.game.playerHand)}"
        el("dealer-score").textContent = s"Dealer: ${handValue(rs.game.dealerHand)}"

  def handleResolution(): Unit =
    showGamePhase()
    setButtons(false, false)
    val originalBet       = rs.game.bet
    val (newGame, payout) = collectWinnings.run(rs.game).value
    update(_.copy(game = newGame))
    val gainStr = if payout - originalBet > 0 then s" +${payout - originalBet}€" else ""
    val msg = rs.game.phase match
      case GamePhase.Resolved(Outcome.PlayerWins)      => s"🎉 Vous gagnez !$gainStr"
      case GamePhase.Resolved(Outcome.PlayerBlackjack) => s"🃏 Blackjack !$gainStr"
      case GamePhase.Resolved(Outcome.DealerWins)      => "😞 Dealer gagne"
      case GamePhase.Resolved(Outcome.PlayerBusts)     => "💥 Bust !"
      case GamePhase.Resolved(Outcome.DealerBusts)     => s"🎉 Dealer bust !$gainStr"
      case GamePhase.Resolved(Outcome.Push)            => "🤝 Égalité"
      case _                                           => ""
    el("result-msg").textContent = msg
    updateScores()

  def updateUI(): Unit =
    updateScores()
    rs.game.phase match
      case GamePhase.Betting =>
        showBettingPhase()
        btn("btn-deal").disabled = rs.game.bet == 0
      case GamePhase.PlayerTurn =>
        showGamePhase()
        setButtons(true, true, rs.game.playerHand.length == 2 && rs.game.balance >= rs.game.bet)
      case GamePhase.DealerTurn =>
        showGamePhase(); setButtons(false, false)
      case GamePhase.Resolved(_) =>
        handleResolution()

  // Apply chip canvas textures to HTML buttons
  List(5, 25, 50, 100).foreach { denom =>
    val b   = document.getElementById(s"chip-$denom").asInstanceOf[dom.html.Element]
    val url = makeChipCanvas(denom).toDataURL("image/png")
    b.style.backgroundImage   = s"url($url)"
    b.style.backgroundSize    = "cover"
    b.style.backgroundColor   = "transparent"
    b.style.border            = "none"
    b.textContent             = ""
  }
  updateUI()

  // Raycaster drag
  val raycaster = new Raycaster()
  val mouse     = new Vector2()

  canvas.addEventListener("mousedown", (e: dom.MouseEvent) =>
    mouse.x =  (e.clientX.toDouble / dom.window.innerWidth)  * 2 - 1
    mouse.y = -(e.clientY.toDouble / dom.window.innerHeight) * 2 + 1
    raycaster.setFromCamera(mouse, camera)
    val hits = raycaster.intersectObjects((rs.cards ++ rs.chips).toJSArray)
    if hits.length > 0 then
      val hit = hits(0).`object`.asInstanceOf[Mesh]
      if rs.chips.contains(hit) then
        dragChips = true; dragMesh = js.undefined
        update(_.copy(velX = 0.0, velY = 0.0))
      else
        dragChips = false; dragMesh = hit
      lastX = e.clientX; lastY = e.clientY
  )

  canvas.addEventListener("mousemove", (e: dom.MouseEvent) =>
    val dx = e.clientX - lastX
    val dy = e.clientY - lastY
    if dragChips && rs.chips.nonEmpty then
      val rx = dy * 0.012; val ry = dx * 0.012
      update { s =>
        val nx = s.rotX + rx; val ny = s.rotY + ry
        s.chips.foreach { m =>
          m.rotation.asInstanceOf[js.Dynamic].x = Pi / 2 + nx
          m.rotation.asInstanceOf[js.Dynamic].y = ny
        }
        s.copy(rotX = nx, rotY = ny, velX = rx, velY = ry)
      }
      lastX = e.clientX; lastY = e.clientY
    else
      dragMesh.foreach { m =>
        m.rotation.y += dx * 0.012
        m.rotation.x += dy * 0.012
        lastX = e.clientX; lastY = e.clientY
      }
  )

  canvas.addEventListener("mouseup", (_: dom.Event) =>
    if dragChips && rs.chips.nonEmpty then
      Animator.add { () =>
        if rs.chips.isEmpty then false
        else
          val vx = rs.velX * 0.88; val vy = rs.velY * 0.88
          if math.abs(vx) > 0.0003 || math.abs(vy) > 0.0003 then
            val nx = rs.rotX + vx; val ny = rs.rotY + vy
            rs.chips.foreach { m =>
              m.rotation.asInstanceOf[js.Dynamic].x = Pi / 2 + nx
              m.rotation.asInstanceOf[js.Dynamic].y = ny
            }
            rs = rs.copy(rotX = nx, rotY = ny, velX = vx, velY = vy)
            true
          else
            rs = rs.copy(velX = 0.0, velY = 0.0)
            false
      }
    dragMesh = js.undefined; dragChips = false
  )

  // Chip buttons
  List(5, 25, 50, 100).foreach { denom =>
    document.getElementById(s"chip-$denom").addEventListener("click", (_: dom.Event) =>
      if rs.game.phase == GamePhase.Betting && rs.game.balance >= denom then
        val (newGame, _) = placeBet(denom).run(rs.game).value
        update(s => addChip(denom)(s.copy(game = newGame)))
        updateUI()
    )
  }

  btn("btn-clear-bet").addEventListener("click", (_: dom.Event) =>
    if rs.game.phase == GamePhase.Betting then
      val (newGame, _) = clearBet.run(rs.game).value
      update(s => clearChips(s.copy(game = newGame)))
      updateUI()
  )

  btn("btn-deal").addEventListener("click", (_: dom.Event) =>
    if rs.game.phase == GamePhase.Betting && rs.game.bet > 0 then
      val (newGame, _) = dealInitialCards.run(rs.game).value
      update(s => deal(s.copy(game = newGame)))
      updateUI()
  )

  btn("btn-hit").addEventListener("click", (_: dom.Event) =>
    rs.game.phase match
      case GamePhase.PlayerTurn =>
        val (newGame, _) = playerHit.run(rs.game).value
        update(s => hit(newGame.playerHand.last)(s.copy(game = newGame)))
        updateUI()
      case _ => ()
  )

  btn("btn-double").addEventListener("click", (_: dom.Event) =>
    rs.game.phase match
      case GamePhase.PlayerTurn if rs.game.playerHand.length == 2 && rs.game.balance >= rs.game.bet =>
        setButtons(false, false)
        val originalBet    = rs.game.bet
        val (newGame, _)   = doubleDown.run(rs.game).value
        update(s => hit(newGame.playerHand.last)(addChipsForAmount(originalBet)(s).copy(game = newGame)))
        rs.game.phase match
          case GamePhase.Resolved(Outcome.PlayerBusts) => updateUI()
          case _ =>
            rs.holeCard match
              case hc if !js.isUndefined(hc) =>
                animateFlip(hc.asInstanceOf[Mesh], () =>
                  update(dealerReveal)
                  updateUI()
                )
              case _ =>
                update(dealerReveal)
                updateUI()
      case _ => ()
  )

  btn("btn-stand").addEventListener("click", (_: dom.Event) =>
    rs.game.phase match
      case GamePhase.PlayerTurn =>
        setButtons(false, false)
        rs.holeCard match
          case hc if !js.isUndefined(hc) =>
            animateFlip(hc.asInstanceOf[Mesh], () =>
              val (newGame, _) = playerStand.run(rs.game).value
              update(s => dealerReveal(s.copy(game = newGame)))
              updateUI()
            )
          case _ =>
            val (newGame, _) = playerStand.run(rs.game).value
            update(s => dealerReveal(s.copy(game = newGame)))
            updateUI()
      case _ => ()
  )

  btn("btn-new").addEventListener("click", (_: dom.Event) =>
    update(newRound)
    updateUI()
  )
