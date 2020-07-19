package icfpc.classified.replay

import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Font}
import icfpc.classified.game.{Vector, WorldState}
import scala.util.{Failure, Success, Try}

object WordRenderer {
  val size = 800
  val scale = 4

  implicit class RichVector(val vector: Vector) {

    def toScreen: Vector = {
      Vector(
        x = vector.x * scale + (size / 2),
        y = vector.y * scale + (size / 2)
      )

    }
  }

  def render(states: Seq[Try[WorldState]]): Seq[BufferedImage] = {
    states.map {
      case Failure(exception) => renderError(exception)
      case Success(value) => renderWorld(value)
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

  def renderWorld(state: WorldState): BufferedImage = {
    val image = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
    val g = image.createGraphics()
    g.setFont(new Font("Monospaced", Font.PLAIN, 14))
    val halfShipSize = (2.5 * scale).toInt

    //Background
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, size, size)

    //BlackHole
    g.setColor(Color.WHITE)
    g.fillOval(size / 2 - 10, size / 2 - 10, 20, 20)
    val topLeft = Vector(-14, -14).toScreen
    g.drawRect(topLeft.x, topLeft.y, 28 * scale, 28 * scale)

    //Attacker
    g.setColor(Color.GREEN)
    drawString(g, pprint.apply(state.attacker, 20).plainText.replace("Actor", "Attacker"), 600, 20)
    g.setStroke(3)
    if (state.isDefence) g.setColor(Color.RED) else g.setColor(Color.GREEN)
    val aPos = state.attacker.position.toScreen
    g.drawLine(aPos.x, aPos.y - halfShipSize, aPos.x - halfShipSize, aPos.y + halfShipSize)
    g.drawLine(aPos.x - halfShipSize, aPos.y + halfShipSize, aPos.x + halfShipSize, aPos.y + halfShipSize)
    g.drawLine(aPos.x + halfShipSize, aPos.y + halfShipSize, aPos.x, aPos.y - halfShipSize)
    val aSpeed = state.attacker.speed
    if (aSpeed.nonZero) {
      val newPos = (state.attacker.position + aSpeed).toScreen
      g.setColor(Color.YELLOW)
      g.setStroke(1)
      g.drawLine(aPos.x, aPos.y, newPos.x, newPos.y)
    }

    //Defender
    g.setColor(Color.GREEN)
    drawString(g, pprint.apply(state.defender, 20).plainText.replace("Actor", "Defender"), 20, 20)
    g.setStroke(3)
    if (!state.isDefence) g.setColor(Color.RED) else g.setColor(Color.GREEN)
    val dPos = state.defender.position.toScreen
    g.drawRect(dPos.x - halfShipSize, dPos.y - halfShipSize, halfShipSize * 2, halfShipSize * 2)
    val dSpeed = state.defender.speed
    if (dSpeed.nonZero) {
      val newPos = (state.defender.position + dSpeed).toScreen
      g.setColor(Color.YELLOW)
      g.setStroke(1)
      g.drawLine(dPos.x, dPos.y, newPos.x, newPos.y)
    }

    g.dispose()
    image
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
