package icfpc.classified.game

import java.awt.event.{KeyEvent, KeyListener, MouseEvent, MouseListener, WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Color, Dimension, Graphics}
import java.awt.image.BufferedImage

import icfpc.classified.GraphicsUtils
import icfpc.classified.Renderer.{renderSeq, MyPlane}
import javax.swing.{JFrame, JPanel}

object ShapeCreator extends App {
  val width = 3
  val height = 3
  val frame = new JFrame("ShapeCreator")
  frame.setLayout(new BorderLayout())
  frame.setVisible(true)
  val plane = DrawPlane(emptyImage(width, height))
  frame.add(plane);
  frame.pack();
  frame.setLocationRelativeTo(null)
  plane.addMouseListener(new MouseListener {
    override def mouseClicked(e: MouseEvent): Unit = {}

    override def mousePressed(e: MouseEvent): Unit = {
      //        println("BUSY")
      val x = e.getX / plane.scale
      val y = e.getY / plane.scale

      plane.image = flipPixel(plane.image, x, y)
      plane.repaint()
    }

    override def mouseReleased(e: MouseEvent): Unit = ()

    override def mouseEntered(e: MouseEvent): Unit = ()

    override def mouseExited(e: MouseEvent): Unit = ()
  })
  frame.addWindowListener(new WindowAdapter {

    override def windowClosing(e: WindowEvent): Unit = {
      synchronized {
        notify()
      }
    }
  })
  frame.addKeyListener(new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = ()

    override def keyPressed(e: KeyEvent): Unit = {
      if (e.getKeyChar == 's' || e.getKeyChar == 'S') {
        javax.imageio.ImageIO.write(plane.image, "png", new java.io.File("drawing.png"))
      }
    }

    override def keyReleased(e: KeyEvent): Unit = ()
  })
  synchronized {
    wait()
  }

  def emptyImage(width: Int, height: Int) = {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY)
    val g = image.createGraphics()
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, width, height)
    g.dispose()
    image
  }

  def flipPixel(image: BufferedImage, x: Int, y: Int): BufferedImage = {
    val old = image.getRGB(x, y)
    val g = image.getGraphics
    if (old == GraphicsUtils.BLACK_RGB) {
      g.setColor(Color.WHITE)
    } else {
      g.setColor(Color.BLACK)
    }
    g.drawLine(x, y, x, y)
    g.dispose()
    image
  }

  case class DrawPlane(var image: BufferedImage, var scale: Int = 32) extends JPanel {

    override def getPreferredSize: Dimension = {
      new Dimension(image.getWidth * scale, image.getHeight * scale)
    }

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      g.drawImage(image, 0, 0, image.getWidth * scale, image.getHeight() * scale, null)
    }
  }

}
