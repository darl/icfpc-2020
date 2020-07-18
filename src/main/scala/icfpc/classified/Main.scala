package icfpc.classified

import icfpc.classified.game.Interactor

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

  val interpreter = Interpreter(
    GalaxyOps.functions,
    ss
  )

  var state: Expression = multiplayerState
  var canvases: Seq[Canvas] = Seq.empty

  println("Joining")
  println("join = " + interactor.join())
  println("start = " + interactor.start(10, 10, 10, 10))
  println("Started")
  println("exec command = " + interactor.command(makeList(0, 0)))

  // ToDo async show
  //  Renderer.show(canvases.map(_.toCanvas))(action)
}
