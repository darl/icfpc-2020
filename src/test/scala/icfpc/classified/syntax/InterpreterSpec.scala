package icfpc.classified.syntax

import icfpc.classified.sandbox.StateAnnotator
import icfpc.classified.{HttpSignalSender, IdentitySignalSender, Renderer}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InterpreterSpec extends AnyWordSpec with Matchers {
  val interpreter = Interpreter(Map.empty, IdentitySignalSender)
  import interpreter._

  "Interpreter" should {
    "apply syntax" in {
      Sum0(1)(2) shouldEqual Apply(Apply(Sum0, 1), 2)
      eval(Sum0(1)(2)) shouldEqual eval(3)
    }
    "number operations" in {
      exec(Apply(Apply(Sum0, 1), 2)).value should be(3)
      exec(Apply(Apply(Sum0, 1), Apply(Negate0, Apply(Negate0, 2)))).value should be(3)

      exec(Apply(Apply(Sum0, 1), 2)).value should be(3)
      exec(Apply(Apply(Div0, 4), 3)).value should be(1)
      exec(Apply(Apply(Mul0, 5), 2)).value should be(10)
      exec(Apply(Power2, 3)).value should be(8)
    }

    "true" in {
      exec(Apply(Apply(True0, 1), 5)) should be(Literal(1))
      exec(Apply(Apply(True0, Apply(Inc0, 5)), True0)) should be(Literal(6))
    }

    "false" in {
      exec(Apply(Apply(False0, 1), 2)) should be(Literal(2))
      exec(Apply(Apply(False0, 1), 2)) should be(exec(Apply(Apply(Apply(SComb0, True0), 1), 2)))
    }

    "Scomb" in {
      eval(Apply(Apply(Apply(SComb0, Sum0), Dec0), 3)) should equal(eval(Apply(Apply(Sum0, 3), Apply(Dec0, 3))))
      exec(Apply(Apply(Apply(SComb0, Sum0), Inc0), 1)) should equal(Literal(3))
      eval(Apply(Apply(Apply(SComb0, True0), Sum0), 1)) should equal(eval(Apply(Apply(False0, Sum0), 1)))
    }

    "Bcomb" in {
      exec(Apply(Apply(Apply(BComb0, Dec0), Inc0), 1)) should equal(Literal(1))
      exec(Apply(Apply(Sum0, Apply(Apply(Apply(BComb0, Dec0), Inc0), 1)), 2)) should equal(Literal(3))
    }

    "CComb" in {
      exec(Apply(Apply(Apply(BComb0, Dec0), Inc0), 1)) should equal(Literal(1))
      exec(Apply(Apply(Sum0, Apply(Apply(Apply(BComb0, Dec0), Inc0), 1)), 2)) should equal(Literal(3))
    }

    "List stuff" in {
      eval(makeList(1, 2, 3)) shouldEqual eval(Cons(1, Cons(2, Cons(3, Nil))))

      exec(Apply(Car, makeList(2, 1))) should equal(Literal(2))
      eval(Apply(Cdr, makeList(1, 2, 3))) should equal(eval(Cons(2, Cons(3, Nil))))
      exec(Apply(Apply(Apply(IsNil, Apply(Cdr, Cons(1, Nil))), 1), 2)) should equal(Literal(1))

      eval(makeList(1, 2, 3)).toList shouldBe List(Literal(1), Literal(2), Literal(3))
    }

    "Cons" in {
      eval(Apply(Cons0, 1)) shouldEqual eval(Cons1(1))
      eval(Apply(Apply(Cons0, 1), Nil)) shouldEqual Cons(1, Nil)
      eval(Apply(Cons1(1), 2)) shouldEqual eval(Cons(1, 2))
      eval(Apply(Apply(Cons0, 1), 2)) shouldEqual eval(Cons(1, 2))
      eval(Apply(Cons1(1), Cons(1, 2))) shouldEqual eval(Cons(1, Cons(1, 2)))
      eval(Apply(Apply(Apply(Cons0, 1), 2), Sum0)) should equal(Literal(3))
    }

    "logic" in {
      eval(Apply(Apply(Apply(IfZero0, 0), Sum0), Div1(3))) should equal(Sum0)
      eval(Apply(Apply(Apply(IfZero0, 1), Sum0), Div1(3))) should equal(Div1(3))

      eval(Apply(Apply(EqualTo0, 0), 0)) should equal(True0)
      eval(Apply(Apply(EqualTo0, 0), 1)) should equal(False0)

    }

    "Draw" in {
      eval(Apply(Draw, Nil)) should equal(Canvas(List.empty))
      eval(Apply(Draw, Cons(Cons(1, 2), Nil))) should equal(Canvas(List(1 -> 2)))
      val canvas = eval(Apply(Draw, Cons(Cons(1, 2), Cons(Cons(2, 3), Nil))))
      canvas.isInstanceOf[Canvas] should equal(true)
      canvas.asInstanceOf[Canvas].points should contain allElementsOf Seq(1 -> 2, 2 -> 3)
    }

    "MultiDraw" in {
      eval(Apply(MultiDraw, Nil)) should equal(Nil)
      eval(Apply(MultiDraw, Cons(Cons(Cons(1, 2), Nil), Nil))) should equal(
        Cons(Canvas(List(1 -> 2)), Nil)
      )
    }

    "eval galaxy.txt" in {
      val int = Interpreter(GalaxyOps.functions, IdentitySignalSender)
      println(int.eval(GalaxyOps.Galaxy(Nil)(pair(0, 0))))
    }

    "interact GALAXY" in {
      val int = Interpreter(
        GalaxyOps.functions,
        new HttpSignalSender("https://icfpc2020-api.testkontur.ru", "8d26edd4434c42df82127c1640bed928")
      )
//      var state: Expression = enterPlayerKey(206919795632185305L)
//      var state: Expression = tutorStart
      var state: Expression = enterTutorLevel(10, int)
//      var state: Expression = Nil

      val res = int.eval(Interact0(GalaxyOps.Galaxy)(state)(pair(0, 0)))
      val (state0, rest) = res.toPair
      state = state0
//      println(state0)
      val canvases = rest.toList.head.toList.map(_.toCanvas)

      Renderer.show(canvases.map(_.toCanvas)) { (x, y) =>
        val res = int.eval(Interact0(GalaxyOps.Galaxy)(state)(pair(x, y)))
        val (state0, rest) = res.toPair
        state = state0
        println("--")
        println(state)
        try {
          println(
            StateAnnotator.annotate(
              state
                .toList(1)
                .toList(9)
                .toList(2)
            )
          )
        } catch {
          case ignored: Throwable => ()
        }

        rest.toList.head.toList.map(_.toCanvas)
      }
    }
  }

  val tutorStart: Cons =
    Cons(
      Literal(2),
      Cons(Cons(Literal(1), Cons(Literal(-1), Nil)), Cons(Literal(0), Cons(Nil, Nil)))
    )

  val multiplayer: Cons = Cons(
    Literal(5),
    Cons(
      Cons(
        Literal(2),
        Cons(
          Literal(0),
          Cons(
            Nil,
            Cons(Nil, Cons(Nil, Cons(Nil, Cons(Nil, Cons(Literal(49870), Nil)))))
          )
        )
      ),
      Cons(Literal(9), Cons(Nil, Nil))
    )
  )

  def enterPlayerKey(playerKey: BigInt): Cons = Cons(
    Literal(5),
    Cons(
      Cons(
        Literal(4),
        Cons(
          Literal(playerKey),
          Cons(
            Nil,
            Cons(Nil,Cons(Nil,Cons(Nil,Cons(Cons(Literal(36),Literal(0)),Cons(Literal(28749),Nil)))))
          )
        )
      ),
      Cons(Literal(9),Cons(Nil,Nil))
    )
  )

  def enterTutorLevel(level: Int, int: Interpreter) = {
    val startState: Expression = multiplayer
    var res = int.eval(Interact0(GalaxyOps.Galaxy)(startState)(pair(0, 0)))
    val (state, _) = res.toPair
    res = int.eval(Interact0(GalaxyOps.Galaxy)(state)(pair(17, 1)))
    val (firstLevel, _) = res.toPair
    val anyLevel =
      Cons(
        firstLevel.toPair._1,
        Cons(
          Cons(
            Literal(level - 1),
            firstLevel.toPair._2.toPair._1.toPair._2
          ),
          firstLevel.toPair._2.toPair._2
        )
      )
    var resultState: Expression = anyLevel
    1 to 8 foreach { i =>
      resultState = int.eval(Interact0(GalaxyOps.Galaxy)(resultState)(pair(0, 0))).toPair._1
    }
    resultState
  }

  val c = Cons(Literal(6),Cons(Cons(Literal(9),Cons(Literal(8),Cons(Literal(8200354980347707928L),Cons(Literal(1),Cons(Literal(0),Cons(Literal(0),Cons(Nil,Cons(Nil,Cons(Literal(4),Cons(Cons(Literal(0),Cons(Nil,Cons(Cons(Cons(Cons(Literal(1),Cons(Literal(0),Cons(Cons(Literal(16),Literal(0)),Cons(Cons(Literal(1),Literal(0)),Cons(Cons(Literal(0),Cons(Literal(0),Cons(Literal(0),Cons(Literal(1),Nil)))),Cons(Literal(0),Cons(Literal(64),Cons(Literal(1),Nil)))))))),Cons(Nil,Nil)),Nil),Nil))),Cons(Cons(Literal(8),Cons(Literal(1),Cons(Cons(Literal(448),Cons(Literal(1),Cons(Literal(64),Nil))),Cons(Nil,Cons(Nil,Nil))))),Cons(Nil,Cons(Nil,Nil))))))))))))),Cons(Literal(9),Cons(Nil,Nil))))
}
