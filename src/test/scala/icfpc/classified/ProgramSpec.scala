package icfpc.classified

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ProgramSpec extends AnyWordSpec with Matchers {
  "Program" should {
    "evaluate to 1" in {
      Program(Variable(1)).execute should be(1)
    }

    "sum to Literals" in {
      Program(Apply(Sum(1), 2)).execute should be(3)
    }
  }
}
