package icfpc.classified

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InterpreterSpec extends AnyWordSpec with Matchers {
  import Interpreter._

  "Interpreter" should {
    "sum 2 numbers" in {
      Interpreter.exec(Apply(Apply(Sum0, 1), 2)).value should be(3)
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
      exec(Apply(Apply(Apply(SComb0, Sum0), Dec0), 3)) should equal(exec(Apply(Apply(Sum0, 3), Apply(Dec0, 3))))
      exec(Apply(Apply(Apply(SComb0, Sum0), Inc0), 1)) should equal(Literal(3))
    }

    "Bcomb" in {
      exec(Apply(Apply(Apply(BComb0, Dec0), Inc0), 1)) should equal(Literal(1))
    }

    "Car" in {
      exec(Apply(Car, Apply(Apply(Apply(Cons0, Nil), 1), 2))) should equal(Literal(2))
      exec(Apply(Apply(Apply(IsNil, Apply(Cdr, Cons(1, Nil))), 1), 2)) should equal(Literal(1))
    }
  }
}
