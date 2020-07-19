package icfpc.classified.sandbox

import icfpc.classified.replay.{ReplayParser, ReplayPlayer, StateCapture}
import icfpc.classified.{HttpSignalSender, Player}
import icfpc.classified.syntax.{pair, Demodulator, GalaxyOps, Interact0, Interpreter, Literal}

object Match extends App {
  val address = "https://icfpc2020-api.testkontur.ru"

  val (attackerId, defenderId) = requestGame()

  val t1 = new Thread(() => {
    val capture = StateCapture.mutable
    try {
      Player.play(address, attackerId.value)(capture)
    } catch {
      case err: Throwable => err.printStackTrace()
    }
//    ReplayPlayer(ReplayParser.render(capture.states)).show()
    capture.states.foreach { state =>
      Console.println("-----------------------------------")
      Console.println(StateAnnotator.annotate(state))
    }
  })

  val t2 = new Thread(() => {
    try {
      Player.play(address, defenderId.value)(StateCapture.noOp)
    } catch {
      case err: Throwable => err.printStackTrace()
    }
  })
  t1.start()
  t2.start()
  t1.join()
  t2.join()

  def requestGame(): (Literal, Literal) = {
    val requestState = Demodulator.demodulate(
      "1101100101111101100011110101100110011001100111101110001001000101101111101001110000111000001101101001110000"
    )
    val interpreter = Interpreter(
      GalaxyOps.functions,
      new HttpSignalSender(address, "8d26edd4434c42df82127c1640bed928")
    )
    val result = interpreter.eval(Interact0(GalaxyOps.Galaxy)(requestState)(pair(45, -1)))

    val state = result.toPair._1.toList(1).toList(3).toList(1).toList
    (state(0).toList(1).toLiteral, state(1).toList(1).toLiteral)
  }
}
