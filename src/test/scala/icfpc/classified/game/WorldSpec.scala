package icfpc.classified.game

import org.scalatest.wordspec.AnyWordSpec
import icfpc.classified.syntax._
import org.scalatest.matchers.should.Matchers

class WorldSpec extends AnyWordSpec with Matchers {
  "WorldState" should {
    "parse start state" in {
      val start = Cons(
        Literal(1),
        Cons(
          Literal(1),
          Cons(
            Cons(
              Literal(256),
              Cons(
                Literal(0),
                Cons(
                  Cons(Literal(512), Cons(Literal(1), Cons(Literal(64), Nil))),
                  Cons(
                    Cons(Literal(16), Cons(Literal(128), Nil)),
                    Cons(Cons(Literal(1), Cons(Literal(2), Cons(Literal(3), Cons(Literal(4), Nil)))), Nil)
                  )
                )
              )
            ),
            Cons(
              Cons(
                Literal(0),
                Cons(
                  Cons(Literal(16), Cons(Literal(128), Nil)),
                  Cons(
                    Cons(
                      Cons(
                        Cons(
                          Literal(1),
                          Cons(
                            Literal(0),
                            Cons(
                              Cons(Literal(22), Literal(48)),
                              Cons(
                                Cons(Literal(0), Literal(0)),
                                Cons(
                                  Cons(Literal(1), Cons(Literal(2), Cons(Literal(3), Cons(Literal(4), Nil)))),
                                  Cons(Literal(0), Cons(Literal(64), Cons(Literal(1), Nil)))
                                )
                              )
                            )
                          )
                        ),
                        Cons(Nil, Nil)
                      ),
                      Cons(
                        Cons(
                          Cons(
                            Literal(0),
                            Cons(
                              Literal(1),
                              Cons(
                                Cons(Literal(-22), Literal(-48)),
                                Cons(
                                  Cons(Literal(0), Literal(0)),
                                  Cons(
                                    Cons(
                                      Literal(10),
                                      Cons(Literal(10), Cons(Literal(10), Cons(Literal(10), Nil)))
                                    ),
                                    Cons(Literal(0), Cons(Literal(64), Cons(Literal(1), Nil)))
                                  )
                                )
                              )
                            )
                          ),
                          Cons(Nil, Nil)
                        ),
                        Nil
                      )
                    ),
                    Nil
                  )
                )
              ),
              Nil
            )
          )
        )
      )

      val world = WorldState.parse(start)
      world.defender.position.x should be(22)
    }
  }
}
