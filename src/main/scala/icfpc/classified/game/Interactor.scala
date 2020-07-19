package icfpc.classified.game

import icfpc.classified._
import icfpc.classified.replay.StateCapture
import icfpc.classified.syntax._

class Interactor(signalSender: SignalSender, playerKey: Long) {

  private def sendReceive(exp: Expression): Expression = {
    val expression = Modulator.modulate(exp)
    val res = signalSender.send(expression)
    println(s"response: $res")
    Demodulator.demodulate(res)
  }

  def join()(implicit stateCapture: StateCapture): Expression = {
    sendReceive(makeList(2L, playerKey, Nil))
  }

  def start(n1: Int, n2: Int, n3: Int, n4: Int): Expression = {
    sendReceive(makeList(3L, playerKey, makeList(n1, n2, n3, n4)))
  }

  def command(commands: Expression): Expression = {
    sendReceive(makeList(4L, playerKey, commands))
  }
}
