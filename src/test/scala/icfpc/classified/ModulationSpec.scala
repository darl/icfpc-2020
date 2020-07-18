package icfpc.classified

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ModulationSpec extends AnyWordSpec with Matchers {
  "Modulator" should {
    "Do simple literal modulation" in {
      Modulator.modulate(Literal(0)) should be("010")
      Modulator.modulate(Literal(1)) should be("01100001")
      Modulator.modulate(Literal(-1)) should be("10100001")
      Modulator.modulate(Literal(7)) should be("01100111")
      Modulator.modulate(Literal(16)) should be("0111000010000")
      Modulator.modulate(Literal(-16)) should be("1011000010000")
      Modulator.modulate(Literal(255)) should be("0111011111111")
      Modulator.modulate(Literal(256)) should be("011110000100000000")
    }

    "Do list modulation" in {
      // Nil
      Modulator.modulate(Nil) should be("00")
      // List(Nil)
      Modulator.modulate(Cons(Nil, Nil)) should be("110000")
      // List(0, Nil)
      Modulator.modulate(Cons(Literal(0), Nil)) should be("1101000")
      // List(1, 2)
      Modulator.modulate(Cons(Literal(1), Literal(2))) should be("110110000101100010")
      // List(1, 2, Nil)
      Modulator.modulate(Cons(Literal(1), Cons(Literal(2), Nil))) should be("1101100001110110001000")
      // List(1, List(2, 3, Nil), 4, Nil)
      Modulator.modulate(
        Cons(Literal(1), Cons(Cons(Literal(2), Cons(Literal(3), Nil)), Cons(Literal(4), Nil)))
      ) should be("1101100001111101100010110110001100110110010000")
    }

    "Do simple literal demodulation" in {
      Demodulator.demodulate("010") should be(Literal(0))
      Demodulator.demodulate("01100001") should be(Literal(1))
      Demodulator.demodulate("10100001") should be(Literal(-1))
      Demodulator.demodulate("01100111") should be(Literal(7))
      Demodulator.demodulate("0111000010000") should be(Literal(16))
      Demodulator.demodulate("1011000010000") should be(Literal(-16))
      Demodulator.demodulate("0111011111111") should be(Literal(255))
      Demodulator.demodulate("011110000100000000") should be(Literal(256))
    }

    "Do list demodulation" in {
      Demodulator.demodulate("00") should be(Nil)
      Demodulator.demodulate("110000") should be(Cons(Nil, Nil))
      Demodulator.demodulate("1101000") should be(Cons(Literal(0), Nil))
      Demodulator.demodulate("110110000101100010") should be(Cons(Literal(1), Literal(2)))
      Demodulator.demodulate("1101100001110110001000") should be(Cons(Literal(1), Cons(Literal(2), Nil)))
      Demodulator.demodulate("1101100001111101100010110110001100110110010000") should be(
        Cons(Literal(1), Cons(Cons(Literal(2), Cons(Literal(3), Nil)), Cons(Literal(4), Nil)))
      )
    }
  }
}
