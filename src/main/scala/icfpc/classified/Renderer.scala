package icfpc.classified

import java.awt.event.{MouseEvent, MouseListener, WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Color, Dimension, Graphics, Image}
import java.awt.image.BufferedImage

import javax.swing.{JFrame, JPanel, WindowConstants}

import scala.util.Random

object Renderer {

  case class Rendered(image: BufferedImage, minX: Int, minY: Int, width: Int, height: Int)

  def render(canvases: Canvas*): Rendered = renderSeq(canvases)

  def renderSeq(canvases: Seq[Canvas]): Rendered = {
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

    Rendered(image, minX, minY, width, height)
  }

  case class MyPlane(var image: BufferedImage) extends JPanel {
    val Scale = 32

    override def getPreferredSize: Dimension = {
      new Dimension(image.getWidth * Scale, image.getHeight * Scale)
    }

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      g.drawImage(image, 0, 0, image.getWidth * Scale, image.getHeight() * Scale, null)
    }
  }

  def show(canvases: Seq[Canvas])(onClick: (Int, Int) => Seq[Canvas]): JFrame = {
    var rendered = renderSeq(canvases)

    val frame = new JFrame("Galaxy");
    frame.setLayout(new BorderLayout());
    val plane = MyPlane(rendered.image)
    frame.add(plane);
    frame.pack();
    frame.setLocationRelativeTo(null);
    frame.setVisible(true);
    frame.setResizable(false)

    val lock = new Object

    plane.addMouseListener(new MouseListener {
      override def mouseClicked(e: MouseEvent): Unit = {}

      override def mousePressed(e: MouseEvent): Unit = {
        val x = e.getX / plane.Scale + rendered.minX
        val y = e.getY / plane.Scale + rendered.minY
        println(s"$x $y")
        val newCanvases = onClick(x, y)
        rendered = renderSeq(newCanvases)
        plane.image = rendered.image
        plane.repaint()
      }

      override def mouseReleased(e: MouseEvent): Unit = ()

      override def mouseEntered(e: MouseEvent): Unit = ()

      override def mouseExited(e: MouseEvent): Unit = ()
    })

    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = {
        lock.synchronized {
          lock.notify()
        }
      }
    })
    lock.synchronized {
      lock.wait()
    }
    frame
  }
}
