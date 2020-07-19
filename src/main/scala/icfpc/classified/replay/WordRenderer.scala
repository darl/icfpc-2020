package icfpc.classified.replay

import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Font, Graphics2D}

import icfpc.classified.game.{Actions, Actor, Detonated, Drove, Fired, Vector, WorldState}

import scala.util.{Failure, Success, Try}

object WordRenderer {
  val size = 1200
  val scale = 4

  implicit class RichVector(val vector: Vector) {

    def toScreen: Vector = {
      Vector(
        x = vector.x * scale + (size / 2),
        y = vector.y * scale + (size / 2)
      )

    }
  }

  def render(states: Seq[(Try[WorldState], Actions)]): Seq[BufferedImage] = {
    states.map {
      case (Failure(exception), _) => renderError(exception)
      case (Success(value), command) => renderWorld(value, command)
    }
  }

  def renderError(exception: Throwable): BufferedImage = {
    val image = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
    val g = image.createGraphics()

    //Background
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, size, size)

    //Text
    g.setColor(Color.RED)
    g.setStroke(new BasicStroke(3))
    g.setFont(new Font("Monospaced", Font.PLAIN, 36))
    g.drawString("ERROR. see console", 100, 300)
    exception.printStackTrace()

    g.dispose()
    image
  }

  def renderWorld(state: WorldState, command: Actions): BufferedImage = {
    val image = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
    val g = image.createGraphics()
    g.setFont(new Font("Monospaced", Font.PLAIN, 14))
    val halfShipSize = (2.5 * scale).toInt
    val halfAddShipSize = 2 * scale

    //Background
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, size, size)

    //BlackHole
    g.setColor(Color.WHITE)
    g.fillOval(size / 2 - 10, size / 2 - 10, 20, 20)
    val topLeft = Vector(-14, -14).toScreen
    g.drawRect(topLeft.x.toInt, topLeft.y.toInt, 28 * scale, 28 * scale)

    //Attacker
    g.setColor(Color.GREEN)
    drawString(g, pprint.apply(state.attacker, 20).plainText.replace("Actor", "Attacker"), 1000, 20)
    g.setStroke(3)
    if (state.me == state.defender) g.setColor(Color.RED) else g.setColor(Color.GREEN)
    val aPos = state.attacker.position.toScreen
    g.drawLine(aPos.x.toInt, aPos.y.toInt - halfShipSize, aPos.x.toInt - halfShipSize, aPos.y.toInt + halfShipSize)
    g.drawLine(
      (aPos.x.toInt - halfShipSize),
      aPos.y.toInt + halfShipSize,
      aPos.x.toInt + halfShipSize,
      aPos.y.toInt + halfShipSize
    )
    g.drawLine(aPos.x.toInt + halfShipSize, aPos.y.toInt + halfShipSize, aPos.x.toInt, aPos.y.toInt - halfShipSize)
    val aSpeed = state.attacker.speed
    if (aSpeed.nonZero) {
      val newPos = (state.attacker.position + aSpeed).toScreen
      g.setColor(Color.YELLOW)
      g.setStroke(1)
      g.drawLine(aPos.x.toInt, aPos.y.toInt, newPos.x.toInt, newPos.y.toInt)
    }
    drawActions(g, state.attacker)
    drawTrajectory(g, state.attacker)

    //Defender
    g.setColor(Color.GREEN)
    drawString(g, pprint.apply(state.defender, 20).plainText.replace("Actor", "Defender"), 20, 20)
    g.setStroke(3)
    if (!state.isDefence) g.setColor(Color.RED) else g.setColor(Color.GREEN)
    val dPos = state.defender.position.toScreen
    g.drawRect(dPos.x.toInt - halfShipSize, dPos.y.toInt - halfShipSize, halfShipSize * 2, halfShipSize * 2)
    val dSpeed = state.defender.speed
    if (dSpeed.nonZero) {
      val newPos = (state.defender.position + dSpeed).toScreen
      g.setColor(Color.YELLOW)
      g.setStroke(1)
      g.drawLine(dPos.x.toInt, dPos.y.toInt, newPos.x.toInt, newPos.y.toInt)
    }
    drawActions(g, state.defender)
    drawTrajectory(g, state.defender)

    // All other ships
    g.setColor(Color.ORANGE)
    g.setStroke(3)
    state.adds.foreach { a =>
      val aPos = a.position.toScreen
      g.fillRect(aPos.x.toInt - halfAddShipSize, aPos.y.toInt - halfAddShipSize, halfAddShipSize * 2, halfAddShipSize * 2)
    }

    //Me
    g.setStroke(1)
    g.setColor(Color.GREEN)
    drawString(g, pprint.apply(command, 20).plainText, 20, 900)
    drawString(g, pprint.apply(state.debug, 20).plainText, 900, 900)

    g.dispose()
    image
  }

  private def drawActions(g: Graphics2D, actor: Actor): Unit = {
    import Color._
    val aPos = actor.position.toScreen
    actor.performedActions.foreach {
      case Fired(target, x1, x2, x3) =>
        g.setColor(new Color(MAGENTA.getRed, MAGENTA.getGreen, MAGENTA.getBlue, 128))
        val targetPos = target.toScreen
        g.drawLine(aPos.x.toInt, aPos.y.toInt, targetPos.x.toInt, targetPos.y.toInt)
      case Drove(horizontal, vertical) =>
        g.setColor(CYAN)
        g.setStroke(2)
        val d = Vector(horizontal, vertical)
        val a = (actor.position + (d * 6)).toScreen
        val b = (actor.position + (d * 10)).toScreen
        g.drawLine(a.x.toInt, a.y.toInt, b.x.toInt, b.y.toInt)
      case Detonated =>
        val halfSize = (3.5 * scale).toInt
        g.setColor(WHITE)
        g.setStroke(4)
        g.fillRoundRect(aPos.x.toInt - halfSize, aPos.y.toInt - halfSize, halfSize * 2, halfSize * 2, 10, 10)
    }
  }

  def drawTrajectory(g: Graphics2D, actor: Actor): Unit = {
    g.setStroke(1)
    g.setColor(Color.LIGHT_GRAY)
    actor.trajectory.next(10).foldLeft(actor.position) {
      case (a, b) =>
        val left = a.toScreen
        val right = b.position.toScreen
        g.drawLine(left.x.toInt, left.y.toInt, right.x.toInt, right.y.toInt)
        b.position
    }
  }

  import java.awt.Graphics

  private def drawString(g: Graphics, text: String, x: Int, y: Int): Unit = {
    val lineHeight = g.getFontMetrics.getHeight
    text.split("\n").zipWithIndex.foreach {
      case (line, i) =>
        g.drawString(line, x, y + i * lineHeight)
    }
  }

  implicit def intToStroke(stroke: Int): BasicStroke = new BasicStroke(stroke)
}
