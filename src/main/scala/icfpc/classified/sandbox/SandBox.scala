package icfpc.classified.sandbox

import icfpc.classified.syntax._

object SandBox extends App {
  println(
    StateAnnotator.annotate(
      Cons(
        Literal(1),
        Cons(
          Literal(0),
          Cons(
            Cons(
              Literal(256),
              Cons(
                Literal(1),
                Cons(
                  Cons(Literal(448), Cons(Literal(1), Cons(Literal(64), Nil))),
                  Cons(Cons(Literal(16), Cons(Literal(128), Nil)), Cons(Nil, Nil))
                )
              )
            ),
            Cons(Nil, Nil)
          )
        )
      )
    )
  )
}
