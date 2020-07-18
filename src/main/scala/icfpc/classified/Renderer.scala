package icfpc.classified

import java.awt.{BorderLayout, Color, Dimension, Graphics, Image}
import java.awt.image.BufferedImage

import javax.swing.{JFrame, JPanel, WindowConstants}

import scala.util.Random

object Renderer {

  def render(canvases: Canvas*): JFrame = renderSeq(canvases)

  def renderSeq(canvases: Seq[Canvas]): JFrame = {
    val minX = canvases.filter(_.nonEmpty).map(_.points.minBy(_._1)._1).min
    val minY = canvases.filter(_.nonEmpty).map(_.points.minBy(_._2)._2).min
    val width = canvases.filter(_.nonEmpty).map(_.points.maxBy(_._1)._1).max - minX + 1
    val height = canvases.filter(_.nonEmpty).map(_.points.maxBy(_._2)._2).max - minY + 1

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g = image.createGraphics()

    // clear background
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, width, height)

    g.setColor(Color.WHITE)
    canvases.foreach { canvas =>
      canvas.points.foreach(p => g.drawLine(-minX + p._1, -minY + p._2, -minX + p._1, -minY + p._2))
      g.setColor(
        new Color(Random.nextInt(128) + 128, Random.nextInt(128) + 128, Random.nextInt(128) + 128, 255 / canvases.size)
      )
    }
    g.dispose()

    javax.imageio.ImageIO.write(image, "png", new java.io.File("drawing.png"))

    val frame = new JFrame("Testing");
//    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    frame.setLayout(new BorderLayout());
    frame.add(MyPlane(image));
    frame.pack();
    frame.setLocationRelativeTo(null);
    frame.setVisible(true);
    frame.setResizable(false)
    frame
  }

  case class MyPlane(image: BufferedImage) extends JPanel {
    val Scale = 8

    override def getPreferredSize: Dimension = {
      new Dimension(image.getWidth * Scale, image.getHeight * Scale)
    }

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      g.drawImage(image, 0, 0, image.getWidth * Scale, image.getHeight() * Scale, null)
    }
  }
}
