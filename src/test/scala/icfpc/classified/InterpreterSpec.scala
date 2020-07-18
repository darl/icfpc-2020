package icfpc.classified

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InterpreterSpec extends AnyWordSpec with Matchers {
  val interpreter = Interpreter(Map.empty)
  import interpreter._

  "Interpreter" should {
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
      exec(Apply(Car, Apply(Apply(Apply(Cons0, Nil), 1), 2))) should equal(Literal(2))
      eval(Apply(Cdr, Apply(Apply(Apply(Cons0, Nil), 1), 2))) should equal((Cons(1, Nil)))
      exec(Apply(Apply(Apply(IsNil, Apply(Cdr, Cons(1, Nil))), 1), 2)) should equal(Literal(1))
    }

    "logic" in {
      eval(Apply(Apply(Apply(IfZero0, 0), Sum0), Div1(3))) should equal(Sum0)
      eval(Apply(Apply(Apply(IfZero0, 1), Sum0), Div1(3))) should equal(Div1(3))

      eval(Apply(Apply(EqualTo0, 0), 0)) should equal(True0)
      eval(Apply(Apply(EqualTo0, 0), 1)) should equal(False0)

    }

    "GALAXY" in {
      val int = Interpreter(GalaxyOps.functions.map(f => f.id -> f.expr).toMap)
      val res = int.exec(Apply(True0, UnknownVariable(1338)))
      Console.println(res)
    }
  }
}
