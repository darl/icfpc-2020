package icfpc.classified.game

import icfpc.classified.GraphicsUtils
import javax.imageio.ImageIO

object ShapeReader {

  def read(name: String): Shape = {
    val image = ImageIO.read(this.getClass.getResourceAsStream(name))
    val points = (0 until image.getWidth).map { x =>
      (0 until image.getHeight).map { y =>
        image.getRGB(x, y) != GraphicsUtils.BLACK_RGB
      }
    }
    Shape(points, image.getWidth, image.getHeight)
  }
}
