package icfpc.classified

import icfpc.classified.syntax.Literal

object AttackerExamples {
  List(
    Literal(1),
    Literal(1),
    List(
      Literal(256),
      Literal(0),
      List(Literal(512), Literal(1), Literal(64)),
      List(Literal(16), Literal(128)),
      List(Literal(1), Literal(2), Literal(3), Literal(4))
    ),
    List(
      Literal(0),
      List(Literal(16), Literal(128)),
      List(
        List(
          List(
            Literal(1),
            Literal(0),
            List(Literal(-48), Literal(36)),
            List(Literal(0), Literal(0)),
            List(Literal(1), Literal(2), Literal(3), Literal(4)),
            Literal(0),
            Literal(64),
            Literal(1)
          ),
          syntax.Nil
        ),
        List(
          List(
            Literal(0),
            Literal(1),
            List(Literal(48), Literal(-36)),
            List(Literal(0), Literal(0)),
            List(Literal(10), Literal(10), Literal(10), Literal(10)),
            Literal(0),
            Literal(64),
            Literal(1)
          ),
          syntax.Nil
        )
      )
    )
  )

  List(
    Literal(1),
    Literal(1),
    List(
      Literal(256),
      Literal(0),
      List(Literal(512), Literal(1), Literal(64)),
      List(Literal(16), Literal(128)),
      List(Literal(112), Literal(64), Literal(4), Literal(16))
    ),
    List(
      Literal(0),
      List(Literal(16), Literal(128)),
      List(
        List(
          List(
            Literal(1),
            Literal(0),
            List(Literal(2), Literal(-48)),
            List(Literal(0), Literal(0)),
            List(Literal(112), Literal(64), Literal(4), Literal(16)),
            Literal(0),
            Literal(64),
            Literal(1)
          ),
          syntax.Nil
        ),
        List(
          List(
            Literal(0),
            Literal(1),
            List(Literal(-2), Literal(48)),
            List(Literal(0), Literal(0)),
            List(Literal(10), Literal(10), Literal(10), Literal(10)),
            Literal(0),
            Literal(64),
            Literal(1)
          ),
          syntax.Nil
        )
      )
    )
  )
}
