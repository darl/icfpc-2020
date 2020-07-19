package icfpc.classified.game

import icfpc.classified._
import icfpc.classified.game.Actor.Stats
import icfpc.classified.syntax._

class Interactor(signalSender: SignalSender, playerKey: Long) {

  private def sendReceive(exp: Expression): Expression = {
    val expression = Modulator.modulate(exp)
    val res = signalSender.send(expression)
    println(s"response: $res")
    Demodulator.demodulate(res)
  }

  def join(): Expression = {
    sendReceive(makeList(2L, playerKey, Nil))
  }

  def start(stats: Stats): Expression = {
    sendReceive(makeList(3L, playerKey, makeList(stats.supply, stats.might, stats.cooling, stats.z)))
  }

  def command(commands: Expression): Expression = {
    sendReceive(makeList(4L, playerKey, commands))
  }
}
