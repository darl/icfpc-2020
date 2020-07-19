package icfpc.classified.replay

import java.awt.BorderLayout
import java.awt.event.{KeyEvent, KeyListener, WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage

import icfpc.classified.Renderer.MyPlane
import icfpc.classified.game.{Actions, WorldState}
import javax.swing.JFrame

import scala.util.{Failure, Success, Try}

case class ReplayPlayer(
    replays: Seq[Try[WorldState]],
    commands: Seq[Actions] = Seq.empty,
    states: Seq[String] = Seq.empty) {

  def show(): Unit = {
    val images: Seq[BufferedImage] = WordRenderer.render(replays.zipAll(commands, null, Actions.empty))
    var current = 0

    val title = replays.last match {
      case Failure(_) => "Error"
      case Success(state) => if (state.me.stats.supply == 0) "Defeat" else "Victory"
    }
    val frame = new JFrame(title);
    frame.setLayout(new BorderLayout());
    val plane = MyPlane(images.head, 1)
    frame.add(plane)
    frame.pack()
    frame.setLocationRelativeTo(null)
    frame.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = ()

      override def keyPressed(e: KeyEvent): Unit = {
        val left = e.getKeyCode == KeyEvent.VK_LEFT && current > 0
        val right = e.getKeyCode == KeyEvent.VK_RIGHT && current < images.size - 1
        if (left) {
          current = current - 1
        }
        if (right) {
          current = current + 1
        }
        if (left || right) {
          plane.image = images(current)
          if (states.nonEmpty) {
            Console.println("-----------------------------------")
            println(states(current))
          }
          plane.repaint()
        }
      }

      override def keyReleased(e: KeyEvent): Unit = ()
    })
    if (states.nonEmpty) {
      Console.println("-----------------------------------")
      println(states(current))
    }
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

}
