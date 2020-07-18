package icfpc.classified

import icfpc.classified.game.WorldState.Started
import icfpc.classified.game.{BotLogic, Interactor, WorldState}

object Main extends App {

  val multiplayerState: Cons = Cons(
    Literal(5),
    Cons(
      Cons(
        Literal(4),
        Cons(
          Literal(BigInt(args(1))),
          Cons(Nil, Cons(Nil, Cons(Nil, Cons(Nil, Cons(Cons(Literal(36), Literal(0)), Cons(Literal(47088), Nil))))))
        )
      ),
      Cons(Literal(9), Cons(Nil, Nil))
    )
  )

  private val ss = new HttpSignalSender(args(0), "8d26edd4434c42df82127c1640bed928")
  private val interactor = new Interactor(ss, args(1).toLong)

  val interpreter = Interpreter(GalaxyOps.functions, ss)
  val bot = new BotLogic

  println("Joining")
  var state: Expression = interactor.join()
  println("join = " + state)
  var world = WorldState.parse(state)
  println("world = " + world)

  state = interactor.start(10, 10, 10, 10)
  println("start = " + state)
  world = WorldState.parse(state)
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
