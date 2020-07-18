package icfpc.classified

import java.awt.{BorderLayout, Color, Dimension, Graphics, Image}
import java.awt.image.BufferedImage

import javax.swing.{JFrame, JPanel, WindowConstants}

object Renderer {

  def render(canvas: Canvas): JFrame = {
    val minX = canvas.points.minBy(_._1)._1
    val minY = canvas.points.minBy(_._2)._2
    val width = canvas.points.maxBy(_._1)._1 - minX + 1
    val height = canvas.points.maxBy(_._2)._2 - minY + 1

    val image = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY)
    val g = image.createGraphics()

    // clear background
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, width, height)

    g.setColor(Color.WHITE)
    canvas.points.foreach(p => g.drawLine(-minX + p._1, -minY + p._2, -minX + p._1, -minY + p._2))
    g.dispose()

    javax.imageio.ImageIO.write(image, "png", new java.io.File("drawing.png"))

    val frame = new JFrame("Testing");
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    frame.setLayout(new BorderLayout());
    frame.add(MyPlane(image));
    frame.pack();
    frame.setLocationRelativeTo(null);
    frame.setVisible(true);
    frame.setResizable(false)
    frame
  }

  case class MyPlane(image: BufferedImage) extends JPanel {

    override def getPreferredSize: Dimension = {
      new Dimension(image.getWidth, image.getHeight)
    }

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      g.drawImage(image, 0, 0, null)
    }
  }
}
