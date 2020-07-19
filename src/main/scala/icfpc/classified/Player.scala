package icfpc.classified

import icfpc.classified.game.WorldState.Started
import icfpc.classified.game.{Actions, BotLogic, Interactor, WorldState}
import icfpc.classified.replay.Capture
import icfpc.classified.syntax._

object Player {

  def play(
      sendAddress: String,
      playerKey: BigInt
    )(implicit stateCapture: Capture[Expression],
      commandCapture: Capture[Actions]): Unit = {
    val ss = new HttpSignalSender(sendAddress, "8d26edd4434c42df82127c1640bed928")
    val interactor = new Interactor(ss, playerKey.toLong)
    val bot = new BotLogic

    println("Joining")
    var state = interactor.join()
    println("join = " + state)

    // 430 + 4 + 12 + 2 == 448 total
    state = interactor.start(400, 1, 1, 3)
    println("start = " + state)
    var world = WorldState.parse(state)
    println("world = " + world)

    println("Started")
    while (world.status == Started) {
      val actions = bot.run(world)
      commandCapture.log(actions)
      println("actions = " + actions)
      val commands = actions.serialize(world)
      println("sending = " + commands)
      state = interactor.command(commands)
      println("commands = " + state)
      world = WorldState.parse(state)
      println("world = " + world)
    }

    println("exec command = " + interactor.command(makeList(0, 0)))
  }

}
