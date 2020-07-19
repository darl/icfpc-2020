package icfpc.classified.game

import java.awt.BorderLayout
import java.awt.event.{KeyEvent, KeyListener, WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage

import icfpc.classified.Renderer.MyPlane
import javax.swing.JFrame

object ReplayPlayer extends App {
  val replay: Seq[WorldState] = ReplayReader.read("/rep.txt")
  val images: Seq[BufferedImage] = WordRenderer.render(replay)
  var current = 0

  val frame = new JFrame("Replay");
  frame.setLayout(new BorderLayout());
  val plane = MyPlane(images.head, 1)
  frame.add(plane)
  frame.pack()
  frame.setLocationRelativeTo(null)
  frame.addKeyListener(new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = ()

    override def keyPressed(e: KeyEvent): Unit = {
      if (e.getKeyCode == KeyEvent.VK_LEFT && current > 0) {
        current = current - 1
        plane.image = images(current)
        plane.repaint()
      }
      if (e.getKeyCode == KeyEvent.VK_RIGHT && current < images.size - 1) {
        current = current + 1
        plane.image = images(current)
        plane.repaint()
      }
    }

    override def keyReleased(e: KeyEvent): Unit = ()
  })
  frame.setVisible(true)
  val lock = new Object
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
  frame.dispose()
}
