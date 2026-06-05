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

private val PLAYER_ROW   =  0.3
private val DEALER_ROW   =  2.2
private val CHIP_ROW     = -1.8
private val CLUSTER_SPAN =  4.0   // horizontal room shared by split hands
private val CARD_GAP     =  0.5   // gap between cards inside one hand

private case class CameraSetup(px: Double, py: Double, pz: Double,
                                lx: Double, ly: Double, lz: Double)
private val casinoView = CameraSetup(0, -2.0, 6.5, 0, 0.3, 0)

private case class TableConfig(id: String, label: String, minBet: Int, maxBet: Int, chips: List[Int])
private val tables: List[TableConfig] = List(
  TableConfig("bronze", "Bronze",  1,   200,  List(1, 2, 5, 25, 50, 100)),
  TableConfig("silver", "Silver",  5,   400,  List(5, 25, 50, 100)),
  TableConfig("gold",   "Gold",    50,  2000, List(25, 50, 100, 200)),
  TableConfig("vip",    "VIP",     100, 5000, List(50, 100, 200, 500))
)

private def applyCamera(cam: PerspectiveCamera, s: CameraSetup): Unit =
  cam.position.set(s.px, s.py, s.pz)
  cam.lookAt(s.lx, s.ly, s.lz)
  cam.aspect = dom.window.innerWidth.toDouble / dom.window.innerHeight.toDouble
  cam.updateProjectionMatrix()

private case class RenderState(
  game:        GameState,
  playerCards: List[Mesh]       = Nil,
  dealerCards: List[Mesh]       = Nil,
  chips:       List[Mesh]       = Nil,
  holeCard:    js.UndefOr[Mesh] = js.undefined,
  rotX:        Double           = 0.0,
  rotY:        Double           = 0.0,
  velX:        Double           = 0.0,
  velY:        Double           = 0.0
)

def startBlackjack(scene: Scene, camera: PerspectiveCamera, canvas: dom.html.Canvas): Unit =
  scene.clear()
  scene.background = new ThreeColor(0x0d1f0d).asInstanceOf[js.Any]
  applyCamera(camera, casinoView)

  val DECK_X = 3.5
  val DECK_Y = 1.0
  val deckMesh = makeDeckMesh()
  // sit the block so its base is at the table and its top is at DECK_TOP_Z
  deckMesh.position.set(DECK_X, DECK_Y, DECK_VISUAL_D / 2)
  deckMesh.rotation.asInstanceOf[js.Dynamic].set(0.0, 0.0, 0.0)
  scene.add(deckMesh)

  var rs: RenderState = RenderState(bettingState().unsafeRun())
  def update(f: RenderState => RenderState): Unit = rs = f(rs)

  // Interaction state
  var chipsSpread: Boolean = false
  val targetRotations = scala.collection.mutable.Map.empty[Mesh, Double]
  val targetXOffsets = scala.collection.mutable.Map.empty[Mesh, Double]
  val currentXOffsets = scala.collection.mutable.Map.empty[Mesh, Double]

  // Table selection
  var tableConfig: TableConfig = tables.head
  var tableChosen: Boolean     = false

  Animator.add { () =>
    (rs.playerCards ++ rs.dealerCards).foreach { m =>
      val targetRot = targetRotations.getOrElse(m, 0.0)
      m.rotation.z += (targetRot - m.rotation.z) * 0.15
      
      val targetOff = targetXOffsets.getOrElse(m, 0.0)
      val currentOff = currentXOffsets.getOrElse(m, 0.0)
      val newOff = currentOff + (targetOff - currentOff) * 0.15
      m.position.x = m.position.x.asInstanceOf[Double] + (newOff - currentOff)
      currentXOffsets(m) = newOff
    }
    true
  }

  // Cluster `i` of `h` hands is centered here; card `j` of `n` spreads around it.
  def clusterCenter(h: Int, i: Int): Double =
    if h <= 1 then 0.0 else -CLUSTER_SPAN / 2 + i * CLUSTER_SPAN / (h - 1)
  def cardX(center: Double, n: Int, j: Int): Double =
    center - (CARD_GAP / 2.0) + j * CARD_GAP

  // Pure scene transitions: RenderState => RenderState
  // Three.js mutations are side effects at the boundary

  // Initial deal: animate player hand + dealer up-card + face-down hole card.
  def deal: RenderState => RenderState = s =>
    (s.playerCards ++ s.dealerCards).foreach(m => scene.remove(m))
    val hand = s.game.hands.head.cards
    val dh   = s.game.dealerHand
    val pn   = hand.length
    val pMeshes = hand.zipWithIndex.map { (card, i) =>
      val x = cardX(0.0, pn, i)
      val m = makeCardMesh(card)
      m.position.set(x, PLAYER_ROW, i * 0.01); scene.add(m); animateSlide(m, x, PLAYER_ROW, DECK_X, DECK_Y, i * 150.0)
      m
    }
    val up = makeCardMesh(dh(0))
    val ux = cardX(0.0, 2, 0)
    up.position.set(ux, DEALER_ROW, 0.0); scene.add(up); animateSlide(up, ux, DEALER_ROW, DECK_X, DECK_Y, pn * 150.0)
    val hc = makeHoleCardMesh(dh(1))
    val hx = cardX(0.0, 2, 1)
    hc.position.set(hx, DEALER_ROW, 0.01); scene.add(hc); animateSlide(hc, hx, DEALER_ROW, DECK_X, DECK_Y, (pn + 1) * 150.0)
    s.copy(playerCards = pMeshes, dealerCards = List(up, hc), holeCard = hc)

  // Redraw every player hand from state (split-aware); active hand sits forward.
  // Cards listed in `fresh` (hand index, card index) are dealt in from the shoe;
  // the rest are placed instantly so existing cards don't re-fly on every draw.
  def renderPlayer(fresh: Set[(Int, Int)] = Set.empty): RenderState => RenderState = s =>
    s.playerCards.foreach(m => scene.remove(m))
    val hs = s.game.hands
    val H  = hs.length
    var drawOrder = 0
    val meshes = hs.zipWithIndex.flatMap { (hand, hi) =>
      val center   = clusterCenter(H, hi)
      val n        = hand.cards.length
      val isActive = s.game.phase == GamePhase.PlayerTurn && hi == s.game.active
      val y        = if isActive then PLAYER_ROW - 0.2 else PLAYER_ROW
      hand.cards.zipWithIndex.map { (card, ci) =>
        val x = cardX(center, n, ci)
        val m = makeCardMesh(card)
        m.position.set(x, y, ci * 0.01); scene.add(m)
        if fresh.contains((hi, ci)) then
          animateSlide(m, x, y, DECK_X, DECK_Y, drawOrder * 150.0)
          drawOrder += 1
        m
      }
    }
    s.copy(playerCards = meshes)

  // Reveal the dealer's full hand face-up.
  def dealerReveal: RenderState => RenderState = s =>
    s.dealerCards.foreach(m => scene.remove(m))
    val dh = s.game.dealerHand
    val n  = dh.length
    val meshes = dh.zipWithIndex.map { (card, i) =>
      val x = cardX(0.0, n, i)
      val m = makeCardMesh(card)
      m.position.set(x, DEALER_ROW, i * 0.01); scene.add(m)
      if i >= 2 then animateSlide(m, x, DEALER_ROW, DECK_X, DECK_Y, (i - 2) * 200.0)
      m
    }
    s.copy(dealerCards = meshes, holeCard = js.undefined)

  def addChip(denomination: Int): RenderState => RenderState = s =>
    val n    = s.chips.length.toDouble
    val y    = CHIP_ROW + n * 0.07
    val z    = n * 0.001
    val mesh = makeChipMesh(denomination)
    mesh.position.set(0.0, y, z)
    mesh.rotation.asInstanceOf[js.Dynamic].set(Pi / 2 + s.rotX, s.rotY, 0.0)
    scene.add(mesh)
    s.copy(chips = s.chips :+ mesh)

  def clearChips: RenderState => RenderState = s =>
    s.chips.foreach(m => scene.remove(m))
    chipsSpread = false
    s.copy(chips = Nil, rotX = 0.0, rotY = 0.0, velX = 0.0, velY = 0.0)

  def addChipsForAmount(amount: Int): RenderState => RenderState = startState =>
    List(100, 50, 25, 5).foldLeft((amount, startState)) { case ((rem, acc), d) =>
      val newAcc = (1 to (rem / d)).foldLeft(acc)((a, _) => addChip(d)(a))
      (rem % d, newAcc)
    }._2

  def newRound: RenderState => RenderState = s =>
    val cleaned = clearChips(s)
    (cleaned.playerCards ++ cleaned.dealerCards).foreach(m => scene.remove(m))
    cleaned.copy(playerCards = Nil, dealerCards = Nil, holeCard = js.undefined,
                 game = bettingState(s.game.balance, s.game.minBet, s.game.maxBet).unsafeRun())

  // UI helpers

  def el(id: String)  = document.getElementById(id).asInstanceOf[dom.html.Element]
  def btn(id: String) = document.getElementById(id).asInstanceOf[dom.html.Button]

  def setDisplay(id: String, d: String): Unit = el(id).style.display = d

  def setButtons(h: Boolean, s: Boolean, d: Boolean = false,
                 sp: Boolean = false, su: Boolean = false): Unit =
    btn("btn-hit").disabled       = !h
    btn("btn-stand").disabled     = !s
    btn("btn-double").disabled    = !d
    btn("btn-split").disabled     = !sp
    btn("btn-surrender").disabled = !su

  def showBettingPhase(): Unit =
    setDisplay("table-select",       if tableChosen then "none" else "flex")
    setDisplay("betting-controls",   if tableChosen then "flex" else "none")
    setDisplay("controls-bj",        "none")
    setDisplay("insurance-controls", "none")
    setDisplay("label-dealer",       "none")
    setDisplay("label-player",       "none")
    el("dealer-score").textContent = ""
    el("player-score").textContent = ""
    el("result-msg").textContent   = ""

  def showGamePhase(): Unit =
    setDisplay("betting-controls",   "none")
    setDisplay("controls-bj",        "flex")
    setDisplay("insurance-controls", "none")
    setDisplay("label-dealer",       "block")
    setDisplay("label-player",       "block")

  def showInsurancePhase(): Unit =
    setDisplay("betting-controls",   "none")
    setDisplay("controls-bj",        "none")
    setDisplay("insurance-controls", "flex")
    setDisplay("label-dealer",       "block")
    setDisplay("label-player",       "block")
    el("result-msg").textContent = ""

  def playerScoreText: String =
    val hs = rs.game.hands
    if hs.length > 1 then
      if rs.game.phase == GamePhase.PlayerTurn then
        s"Main ${rs.game.active + 1}/${hs.length}: ${handValue(rs.game.activeHand.cards)}"
      else
        "Joueur: " + hs.map(h => handValue(h.cards)).mkString(" / ")
    else
      s"Joueur: ${handValue(hs.head.cards)}"

  def updateScores(): Unit =
    el("balance-display").textContent = s"${rs.game.balance}€"
    el("bet-display").textContent     = s"Mise: ${rs.game.stake}€"
    rs.game.phase match
      case GamePhase.Betting =>
        ()
      case GamePhase.Insurance | GamePhase.PlayerTurn =>
        el("player-score").textContent = playerScoreText
        el("dealer-score").textContent = "Dealer: ?"
      case GamePhase.Resolved =>
        el("player-score").textContent = playerScoreText
        el("dealer-score").textContent = s"Dealer: ${handValue(rs.game.dealerHand)}"

  def outcomeIcon(o: Outcome): String = o match
    case Outcome.PlayerWins | Outcome.DealerBusts => "🎉"
    case Outcome.PlayerBlackjack                  => "🃏"
    case Outcome.PlayerBusts                      => "💥"
    case Outcome.DealerWins                       => "😞"
    case Outcome.Push                             => "🤝"
    case Outcome.Surrender                        => "🏳️"

  def handleResolution(): Unit =
    showGamePhase()
    setButtons(false, false)
    val stakeBefore       = rs.game.stake
    val hadInsurance      = rs.game.insurance > 0
    val multi             = rs.game.hands.length > 1
    val (newGame, payout) = collectWinnings.run(rs.game).value
    update(_.copy(game = newGame))
    val net    = payout - stakeBefore
    val netStr = if net > 0 then s"+$net€" else if net < 0 then s"$net€" else "±0€"
    val msg =
      if multi || hadInsurance then
        val icons = rs.game.hands.map(h => outcomeIcon(h.outcome.get)).mkString(" ")
        s"$icons  $netStr"
      else
        val gainStr = if net > 0 then s" +$net€" else ""
        rs.game.hands.head.outcome.get match
          case Outcome.PlayerWins      => s"🎉 Vous gagnez !$gainStr"
          case Outcome.PlayerBlackjack => s"🃏 Blackjack !$gainStr"
          case Outcome.DealerWins      => "😞 Dealer gagne"
          case Outcome.PlayerBusts     => "💥 Bust !"
          case Outcome.DealerBusts     => s"🎉 Dealer bust !$gainStr"
          case Outcome.Push            => "🤝 Égalité"
          case Outcome.Surrender       => "🏳️ Abandon"
    el("result-msg").textContent = msg
    updateScores()

  def updateUI(): Unit =
    updateScores()
    val broke = rs.game.balance == 0 && rs.game.stake == 0
    setDisplay("btn-reset-credit", if broke then "block" else "none")
    rs.game.phase match
      case GamePhase.Betting =>
        showBettingPhase()
        btn("btn-deal").disabled = rs.game.bet < rs.game.minBet
        tableConfig.chips.foreach { d =>
          document.getElementById(s"chip-$d").asInstanceOf[dom.html.Button].disabled =
            rs.game.balance < d || rs.game.bet + d > rs.game.maxBet
        }
      case GamePhase.Insurance =>
        showInsurancePhase()
        btn("btn-insure-yes").disabled = rs.game.balance < rs.game.bet / 2
      case GamePhase.PlayerTurn =>
        showGamePhase()
        val h          = rs.game.activeHand
        val canDouble  = h.cards.length == 2 && rs.game.balance >= h.bet
        val canSplitNow = canSplit(h.cards) && rs.game.balance >= h.bet && rs.game.hands.length < MAX_HANDS
        val canSurr    = rs.game.hands.length == 1 && h.cards.length == 2
        setButtons(true, true, canDouble, canSplitNow, canSurr)
      case GamePhase.Resolved =>
        handleResolution()

  def scheduleNextRound(): Unit =
    if rs.game.balance > 0 then
      dom.window.setTimeout(() => {
        if rs.game.phase == GamePhase.Resolved then
          update(newRound)
          updateUI()
      }, 3500)

  // Run an action that may end the round: reveal the dealer if so, else refresh.
  def revealAndResolve(): Unit =
    rs.holeCard match
      case hc if !js.isUndefined(hc) =>
        animateFlip(hc.asInstanceOf[Mesh], () =>
          update(dealerReveal)
          updateUI()
          scheduleNextRound()
        )
      case _ =>
        update(dealerReveal)
        updateUI()
        scheduleNextRound()

  def afterAction(): Unit =
    rs.game.phase match
      case GamePhase.Resolved => revealAndResolve()
      case _                  => updateUI()

  // Apply chip canvas textures to HTML buttons
  List(1, 2, 5, 25, 50, 100, 200, 500).foreach { denom =>
    val b = document.getElementById(s"chip-$denom")
    if b != null then
      val el  = b.asInstanceOf[dom.html.Element]
      val url = makeChipCanvas(denom).toDataURL("image/png")
      el.style.backgroundImage   = s"url($url)"
      el.style.backgroundSize    = "cover"
      el.style.backgroundColor   = "transparent"
      el.style.border            = "none"
      el.textContent             = ""
      el.style.display           = "none"
  }
  updateUI()

  // Raycaster interaction
  val raycaster = new Raycaster()
  val mouse     = new Vector2()

  canvas.addEventListener("mousedown", (e: dom.MouseEvent) =>
    mouse.x =  (e.clientX.toDouble / dom.window.innerWidth)  * 2 - 1
    mouse.y = -(e.clientY.toDouble / dom.window.innerHeight) * 2 + 1
    raycaster.setFromCamera(mouse, camera)
    val hits = raycaster.intersectObjects(rs.chips.toJSArray)
    if hits.length > 0 then
      chipsSpread = !chipsSpread
      animateChipSpread(rs.chips, chipsSpread)
  )

  canvas.addEventListener("mousemove", (e: dom.MouseEvent) =>
    mouse.x =  (e.clientX.toDouble / dom.window.innerWidth)  * 2 - 1
    mouse.y = -(e.clientY.toDouble / dom.window.innerHeight) * 2 + 1
    raycaster.setFromCamera(mouse, camera)
    
    val hits = raycaster.intersectObjects((rs.playerCards ++ rs.dealerCards).toJSArray)
    val hitMesh = if hits.length > 0 then Some(hits(0).`object`.asInstanceOf[Mesh]) else None

    var hoveredHandMeshes: List[Mesh] = Nil
    var offset = 0
    var foundPlayerHand = false
    for hand <- rs.game.hands do
      val n = hand.cards.length
      val meshes = rs.playerCards.slice(offset, offset + n)
      if hitMesh.exists(meshes.contains) then
        hoveredHandMeshes = meshes
        foundPlayerHand = true
      offset += n
      
    if !foundPlayerHand && hitMesh.exists(rs.dealerCards.contains) then
      hoveredHandMeshes = rs.dealerCards
      
    (rs.playerCards ++ rs.dealerCards).foreach { m =>
      if hoveredHandMeshes.contains(m) then
        val n = hoveredHandMeshes.length
        val i = hoveredHandMeshes.indexOf(m)
        val angle = (i - (n - 1) / 2.0) * -0.25
        val xOffset = (i - (n - 1) / 2.0) * 0.8
        targetRotations(m) = angle
        targetXOffsets(m) = xOffset
      else
        targetRotations(m) = 0.0
        targetXOffsets(m) = 0.0
    }
  )

  btn("btn-reset-credit").addEventListener("click", (_: dom.Event) =>
    tableChosen = false
    update { s =>
      val cleaned = clearChips(s)
      (cleaned.playerCards ++ cleaned.dealerCards).foreach(m => scene.remove(m))
      cleaned.copy(playerCards = Nil, dealerCards = Nil, holeCard = js.undefined,
                   game = bettingState(1000).unsafeRun())
    }
    updateUI()
  )

  // Table selection
  tables.foreach { t =>
    document.getElementById(s"table-${t.id}").addEventListener("click", (_: dom.Event) =>
      tableConfig = t
      tableChosen = true
      update(s => s.copy(game = bettingState(rs.game.balance, t.minBet, t.maxBet).unsafeRun()))
      List(1, 2, 5, 25, 50, 100, 200, 500).foreach { d =>
        val el = document.getElementById(s"chip-$d")
        if el != null then
          el.asInstanceOf[dom.html.Element].style.display =
            if t.chips.contains(d) then "inline-flex" else "none"
      }
      updateUI()
    )
  }

  // Chip buttons
  List(1, 2, 5, 25, 50, 100, 200, 500).foreach { denom =>
    val el = document.getElementById(s"chip-$denom")
    if el != null then
      el.addEventListener("click", (_: dom.Event) =>
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
    if rs.game.phase == GamePhase.Betting && rs.game.bet >= rs.game.minBet then
      val (newGame, _) = dealInitialCards.run(rs.game).value
      update(s => deal(s.copy(game = newGame)))
      afterAction()
  )

  btn("btn-hit").addEventListener("click", (_: dom.Event) =>
    rs.game.phase match
      case GamePhase.PlayerTurn =>
        val hi           = rs.game.active
        val (newGame, _) = playerHit.run(rs.game).value
        val ci           = newGame.hands(hi).cards.length - 1
        update(s => renderPlayer(Set((hi, ci)))(s.copy(game = newGame)))
        afterAction()
      case _ => ()
  )

  btn("btn-stand").addEventListener("click", (_: dom.Event) =>
    rs.game.phase match
      case GamePhase.PlayerTurn =>
        setButtons(false, false)
        val (newGame, _) = playerStand.run(rs.game).value
        update(s => renderPlayer()(s.copy(game = newGame)))
        afterAction()
      case _ => ()
  )

  btn("btn-double").addEventListener("click", (_: dom.Event) =>
    rs.game.phase match
      case GamePhase.PlayerTurn
        if rs.game.activeHand.cards.length == 2 && rs.game.balance >= rs.game.activeHand.bet =>
        setButtons(false, false)
        val hi           = rs.game.active
        val extra        = rs.game.activeHand.bet
        val (newGame, _) = doubleDown.run(rs.game).value
        val ci           = newGame.hands(hi).cards.length - 1
        update(s => renderPlayer(Set((hi, ci)))(addChipsForAmount(extra)(s).copy(game = newGame)))
        afterAction()
      case _ => ()
  )

  btn("btn-split").addEventListener("click", (_: dom.Event) =>
    rs.game.phase match
      case GamePhase.PlayerTurn
        if canSplit(rs.game.activeHand.cards) && rs.game.balance >= rs.game.activeHand.bet
           && rs.game.hands.length < MAX_HANDS =>
        val hi           = rs.game.active
        val extra        = rs.game.activeHand.bet
        val (newGame, _) = split.run(rs.game).value
        // both resulting hands receive a fresh second card from the shoe
        update(s => renderPlayer(Set((hi, 1), (hi + 1, 1)))(addChipsForAmount(extra)(s).copy(game = newGame)))
        afterAction()
      case _ => ()
  )

  btn("btn-surrender").addEventListener("click", (_: dom.Event) =>
    rs.game.phase match
      case GamePhase.PlayerTurn
        if rs.game.hands.length == 1 && rs.game.activeHand.cards.length == 2 =>
        setButtons(false, false)
        val (newGame, _) = surrender.run(rs.game).value
        update(s => renderPlayer()(s.copy(game = newGame)))
        afterAction()
      case _ => ()
  )

  btn("btn-insure-yes").addEventListener("click", (_: dom.Event) =>
    if rs.game.phase == GamePhase.Insurance then
      val ins          = rs.game.bet / 2
      val (newGame, _) = takeInsurance.run(rs.game).value
      update(s => addChipsForAmount(ins)(s).copy(game = newGame))
      afterAction()
  )

  btn("btn-insure-no").addEventListener("click", (_: dom.Event) =>
    if rs.game.phase == GamePhase.Insurance then
      val (newGame, _) = declineInsurance.run(rs.game).value
      update(_.copy(game = newGame))
      afterAction()
  )

  btn("btn-new").addEventListener("click", (_: dom.Event) =>
    update(newRound)
    updateUI()
  )
