package icfpc.classified.sandbox

import icfpc.classified.game.Actions
import icfpc.classified.replay.{Capture, ReplayParser, ReplayPlayer}
import icfpc.classified.{HttpSignalSender, Player}
import icfpc.classified.syntax.{pair, Demodulator, Expression, GalaxyOps, Interact0, Interpreter, Literal}

object Match extends App {
  val address = "https://icfpc2020-api.testkontur.ru"

  val (defenderId, attackerId) = requestGame()

  val t1 = new Thread(() => {
    val states = Capture.mutable[Expression]
    val commands = Capture.mutable[Actions]
    try {
      Player.play(address, attackerId.value)(states, commands)
    } catch {
      case err: Throwable => err.printStackTrace()
    }
    val annotations = states.elems.map(s => StateAnnotator.annotate(s))
    ReplayPlayer(ReplayParser.render(states.elems), commands.elems, annotations).show()

  })

  val t2 = new Thread(() => {
//    val states = Capture.mutable[Expression]
//    val commands = Capture.mutable[Actions]
    try {
      Player.play(address, defenderId.value)
    } catch {
      case err: Throwable => err.printStackTrace()
    }
//    val annotations = states.elems.map(s => StateAnnotator.annotate(s))
//    ReplayPlayer(ReplayParser.render(states.elems), commands.elems, annotations).show()

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
