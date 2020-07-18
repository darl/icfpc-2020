package icfpc.classified

object DefenderExamples {
  List(
    Literal(1),
    Literal(1),
    List(
      Literal(256),
      Literal(1),
      List(Literal(448), Literal(1), Literal(64)),
      List(Literal(16), Literal(128)),
      Nil
    ),
    List(
      Literal(0),
      List(Literal(16), Literal(128)),
      List(
        List(
          List(
            Literal(1),
            Literal(0),
            List(Literal(48), Literal(31)),
            List(Literal(0), Literal(0)),
            List(Literal(10), Literal(10), Literal(10), Literal(10)),
            Literal(0),
            Literal(64),
            Literal(1)
          ),
          Nil
        ),
        List(
          List(
            Literal(0),
            Literal(1),
            List(Literal(-48), Literal(-31)),
            List(Literal(0), Literal(0)),
            List(Literal(1), Literal(2), Literal(3), Literal(4)),
            Literal(0),
            Literal(64),
            Literal(1)
          ),
          Nil
        )
      )
    )
  )

  List(
    Literal(1),
    Literal(1),
    List(
      Literal(256),
      Literal(1),
      List(Literal(448), Literal(1), Literal(64)),
      List(Literal(16), Literal(128)),
      Nil
    ),
    List(
      Literal(0),
      List(Literal(16), Literal(128)),
      List(
        List(
          List(
            Literal(1),
            Literal(0),
            List(Literal(-24), Literal(-48)),
            List(Literal(0), Literal(0)),
            List(Literal(10), Literal(10), Literal(10), Literal(10)),
            Literal(0),
            Literal(64),
            Literal(1)
          ),
          Nil
        ),
        List(
          List(
            Literal(0),
            Literal(1),
            List(Literal(24), Literal(48)),
            List(Literal(0), Literal(0)),
            List(Literal(112), Literal(64), Literal(4), Literal(16)),
            Literal(0),
            Literal(64),
            Literal(1)
          ),
          Nil
        )
      )
    )
  )
}
