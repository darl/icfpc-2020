package icfpc.classified.game

import icfpc.classified._

class Interactor(signalSender: SignalSender, playerKey: Long) {

  private def sendReceive(exp: Expression): Expression = {
    val expression = Modulator.modulate(exp)
    val res = signalSender.send(expression)
    Demodulator.demodulate(res)
  }

  def join(): Expression = {
    sendReceive(makeList(2, playerKey, Nil))
  }

  def start(n1: Int, n2: Int, n3: Int, n4: Int): Expression = {
    sendReceive(makeList(3, playerKey, makeList(n1, n2, n3, n4)))
  }

  def command(commands: Expression): Expression = {
    sendReceive(makeList(4, playerKey, commands))
  }
}
