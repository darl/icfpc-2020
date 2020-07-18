package icfpc.classified

object Main extends App {
  val start = System.currentTimeMillis()

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

  val interpreter = Interpreter(
    GalaxyOps.functions,
    new HttpSignalSender(args(0), "8d26edd4434c42df82127c1640bed928")
  )
  import interpreter._

  var state: Expression = multiplayerState
  var canvases: Seq[Canvas] = Seq.empty

  def action: (Int, Int) => Seq[Canvas] = { (x, y) =>
    val res = interpreter.eval(Interact0(GalaxyOps.Galaxy)(state)(pair(x, y)))
    val (state0, rest) = res.toPair
    state = state0
    println(state)
    canvases = rest.toList.head.toList.map(_.toCanvas)
    canvases
  }

  action(0, 0)
  val bot = Bot(action)
  bot.joinGame
  println((System.currentTimeMillis() - start) / 1000)

  // ToDo async show
  //  Renderer.show(canvases.map(_.toCanvas))(action)
}
