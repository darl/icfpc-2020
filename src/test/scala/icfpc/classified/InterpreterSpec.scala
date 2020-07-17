package icfpc.classified

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InterpreterSpec extends AnyWordSpec with Matchers {
  "Interpreter" should {
    "sum 2 numbers" in {
      Interpreter.exec(Apply(Apply(Sum0, 1), 2)).value should be(3)
    }
  }
}
