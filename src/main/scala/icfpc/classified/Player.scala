package icfpc.classified

import icfpc.classified.game.WorldState.Started
import icfpc.classified.game.{BotLogic, Interactor, WorldState}
import icfpc.classified.syntax._

import scala.util.{Success, Try}

object Player {

  def play(sendAddress: String, playerKey: BigInt): Seq[Try[WorldState]] = {
    val ss = new HttpSignalSender(sendAddress, "8d26edd4434c42df82127c1640bed928")
    val interactor = new Interactor(ss, playerKey.toLong)
    val bot = new BotLogic
    var states = Seq.empty[Try[WorldState]]
    var state: Expression = Nil

    println("Joining")
    state = interactor.join()
    println("join = " + state)

    state = interactor.start(10, 8, 9, 20)
    println("start = " + state)
    var world = WorldState.parse(state)
    states = states :+ Success(world)
    println("world = " + world)

    println("Started")
    while (world.status == Started && states.last.isSuccess) {
      states = states :+ Try {
        val actions = bot.run(world)
        println("actions = " + actions)
        val commands = actions.serialize
        println("sending = " + commands)
        state = interactor.command(commands)
        println("commands = " + state)
        world = WorldState.parse(state)
        println("world = " + world)
        world
      }
    }

    println("exec command = " + interactor.command(makeList(0, 0)))
    states
  }

}
