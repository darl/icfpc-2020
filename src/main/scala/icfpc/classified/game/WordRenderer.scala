package icfpc.classified.game

import java.awt.{BasicStroke, Color, Polygon, Stroke}
import java.awt.image.BufferedImage

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

  def render(states: Seq[WorldState]): Seq[BufferedImage] = {
    states.map { state =>
      val image = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
      val g = image.createGraphics()
      val halfShipSize = (2.5 * scale).toInt

      //Background
      g.setColor(Color.BLACK)
      g.fillRect(0, 0, size, size)

      //BlackHole
      g.setColor(Color.WHITE)
      g.fillOval(size / 2 - 10, size / 2 - 10, 20, 20)

      //Attacker
      g.setStroke(new BasicStroke(3))
      if (state.isDefence) g.setColor(Color.RED) else g.setColor(Color.GREEN)
      val aPos = state.attacker.position.toScreen
      g.drawLine(aPos.x, aPos.y - halfShipSize, aPos.x - halfShipSize, aPos.y + halfShipSize)
      g.drawLine(aPos.x - halfShipSize, aPos.y + halfShipSize, aPos.x + halfShipSize, aPos.y + halfShipSize)
      g.drawLine(aPos.x + halfShipSize, aPos.y + halfShipSize, aPos.x, aPos.y - halfShipSize)

      //Defender
      g.setStroke(new BasicStroke(3))
      if (!state.isDefence) g.setColor(Color.RED) else g.setColor(Color.GREEN)
      val dPos = state.defender.position.toScreen
      g.drawRect(dPos.x - halfShipSize, dPos.y - halfShipSize, halfShipSize * 2, halfShipSize * 2)

      g.dispose()
      image
    }
  }
}
