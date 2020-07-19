package icfpc.classified

import icfpc.classified.game.WorldState.Started
import icfpc.classified.game.{BotLogic, Interactor, WorldState}
import icfpc.classified.replay.StateCapture
import icfpc.classified.syntax._

object Player {

  def play(sendAddress: String, playerKey: BigInt)(implicit stateCapture: StateCapture): Unit = {
    val ss = new HttpSignalSender(sendAddress, "8d26edd4434c42df82127c1640bed928")
    val interactor = new Interactor(ss, playerKey.toLong)
    val bot = new BotLogic

    println("Joining")
    var state = interactor.join()
    println("join = " + state)

    state = interactor.start(10, 8, 9, 20)
    println("start = " + state)
    var world = WorldState.parse(state)
    println("world = " + world)

    println("Started")
    while (world.status == Started) {
      val actions = bot.run(world)
      println("actions = " + actions)
      val commands = actions.serialize
      println("sending = " + commands)
      state = interactor.command(commands)
      println("commands = " + state)
      world = WorldState.parse(state)
      println("world = " + world)
    }

    println("exec command = " + interactor.command(makeList(0, 0)))
  }

}
