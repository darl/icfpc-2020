package icfpc.classified

import java.awt.event._
import java.awt.image.BufferedImage
import java.awt.{BorderLayout, Color, Dimension, Graphics}

import icfpc.classified.game.{Locator, Shape, ShapeReader}
import javax.swing.{JFrame, JPanel}

import scala.util.Random

object Renderer {
  case class Rendered(image: BufferedImage, minX: Int, minY: Int, width: Int, height: Int)

  def render(canvases: Canvas*): Rendered = renderSeq(canvases)

  def renderSeq(canvases: Seq[Canvas]): Rendered = {
    val minX = canvases.filter(_.nonEmpty).map(_.minX).min
    val minY = canvases.filter(_.nonEmpty).map(_.minY).min
    val width = canvases.filter(_.nonEmpty).map(_.maxX).max - minX + 1
    val height = canvases.filter(_.nonEmpty).map(_.maxY).max - minY + 1

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

  case class MyPlane(var image: BufferedImage, var scale: Int = 4) extends JPanel {

    override def getPreferredSize: Dimension = {
      new Dimension(image.getWidth * scale, image.getHeight * scale)
    }

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      g.drawImage(image, 0, 0, image.getWidth * scale, image.getHeight() * scale, null)
    }
  }

  def show(canvases: Seq[Canvas])(onClick: (Int, Int) => Seq[Canvas]): JFrame = {
    val shipShape: Shape = ShapeReader.read("/ship3.png")
    var rendered = renderSeq(canvases)

    val frame = new JFrame("Galaxy");
    frame.setLayout(new BorderLayout());
    val plane = MyPlane(rendered.image)
    frame.add(plane);
    frame.pack();
    frame.setLocationRelativeTo(null);
    frame.setVisible(true)
    var savedCanvas = canvases.head

    val lock = new Object

    plane.addMouseListener(new MouseListener {
      override def mouseClicked(e: MouseEvent): Unit = {}

      override def mousePressed(e: MouseEvent): Unit = {
//        println("BUSY")
        val x = e.getX / plane.scale + rendered.minX
        val y = e.getY / plane.scale + rendered.minY
        println(s"$x, $y")
        val newCanvases = onClick(x, y)
        savedCanvas = newCanvases.head
        rendered = renderSeq(newCanvases)
        plane.image = rendered.image
        plane.repaint()
//        println("READY")
      }

      override def mouseReleased(e: MouseEvent): Unit = ()

      override def mouseEntered(e: MouseEvent): Unit = ()

      override def mouseExited(e: MouseEvent): Unit = ()
    })

    plane.addMouseWheelListener((e: MouseWheelEvent) => {
      plane.scale = Math.max(2, plane.scale + e.getUnitsToScroll)
      plane.repaint()
    })

    frame.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = ()

      override def keyPressed(e: KeyEvent): Unit = {
        if (e.getKeyChar == '1') {
          println(Locator.find(shipShape, savedCanvas))
        }
      }

      override def keyReleased(e: KeyEvent): Unit = ()
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

///Cons(Literal(4),Cons(Cons(Literal(2),Cons(Cons(Literal(122),Cons(Literal(203),Cons(Literal(410),Cons(Literal(164),Cons(Literal(444),Cons(Literal(484),Cons(Literal(202),Cons(Literal(77),Cons(Literal(251),Cons(Literal(56),Cons(Literal(456),Cons(Literal(435),Cons(Literal(28),Cons(Literal(329),Cons(Literal(257),Cons(Literal(265),Cons(Literal(501),Cons(Literal(18),Cons(Literal(190),Cons(Literal(423),Cons(Literal(384),Cons(Literal(434),Cons(Literal(266),Cons(Literal(69),Cons(Literal(34),Cons(Literal(437),Cons(Literal(203),Cons(Literal(152),Cons(Literal(160),Cons(Literal(425),Cons(Literal(245),Cons(Literal(428),Cons(Literal(99),Cons(Literal(107),Cons(Literal(192),Cons(Literal(372),Cons(Literal(346),Cons(Literal(344),Cons(Literal(169),Cons(Literal(478),Cons(Literal(393),Cons(Literal(502),Cons(Literal(201),Cons(Literal(497),Cons(Literal(313),Cons(Literal(32),Cons(Literal(281),Cons(Literal(510),Cons(Literal(436),Cons(Literal(22),Cons(Literal(237),Cons(Literal(80),Cons(Literal(325),Cons(Literal(405),Cons(Literal(184),Cons(Literal(358),Cons(Literal(57),Cons(Literal(276),Cons(Literal(359),Cons(Literal(189),Cons(Literal(284),Cons(Literal(277),Cons(Literal(198),Cons(Literal(244),Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),Cons(Literal(-1),Cons(Literal(192496425430),Cons(Cons(Literal(34),Cons(Literal(160),Cons(Literal(192),Cons(Literal(384),Cons(Literal(456),Cons(Literal(201),Cons(Literal(437),Cons(Literal(497),Cons(Literal(203),Cons(Literal(203),Cons(Literal(122),Cons(Literal(410),Cons(Literal(77),Cons(Literal(329),Cons(Literal(428),Cons(Literal(107),Nil)))))))))))))))),Nil))))),Cons(Literal(0),Cons(Cons(Literal(192496425430),Nil),Nil))))
