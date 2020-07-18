package icfpc.classified

object Demodulator {

  def demodulate(binary: String): Expression = {
    val result = demodulateNextChunk(binary)
    if (result.tail.nonEmpty) throw new IllegalArgumentException("Illegal binary string")
    result.chunk
  }

  private def demodulateNextChunk(binary: String): DemodulatedChunk =
    if (isLiteralNext(binary)) demodulateLiteral(binary)
    else demodulateList(binary)

  private def demodulateList(binary: String): DemodulatedChunk = {
    if (binary.startsWith("00")) {
      DemodulatedChunk(Nil, binary.substring(2))
    } else {
      val withListData = binary.substring(2)
      val head = demodulateNextChunk(withListData)
      val tail = demodulateNextChunk(head.tail)
      DemodulatedChunk(Cons(head.chunk, tail.chunk), tail.tail)
    }
  }

  private def demodulateLiteral(binary: String): DemodulatedChunk = {
    val (signData, withoutSign) = binary.splitAt(2)
    val sign = signData match {
      case "01" => 1
      case "10" => -1
      case _ => throw new IllegalArgumentException("Illegal literal start")
    }

    val (encodedWidth, withoutWidth) = withoutSign.splitAt(withoutSign.indexOf("0"))
    val valueLength = encodedWidth.length * 4 + 1
    val (encodedValue, tail) = withoutWidth.splitAt(valueLength)
    DemodulatedChunk(Literal(sign * convertToDecimal(encodedValue)), tail)
  }

  private def convertToDecimal(binary: String): Int = {
    val significantPartStart = binary.indexOf("1")
    if (significantPartStart < 0) {
      0
    } else {
      val significantPart = binary.substring(significantPartStart)
      var value = 0
      var power = significantPart.length - 1
      for (s <- significantPart) {
        val digit = s.toString.toInt
        if (digit != 0 && digit != 1) throw new IllegalArgumentException(s"Illegal digit $digit")
        value = value + digit * math.pow(2, power).toInt
        power = power - 1
      }
      value
    }
  }

  private def isLiteralNext(binary: String): Boolean =
    binary.startsWith("01") || binary.startsWith("10")

  case class DemodulatedChunk(chunk: Expression, tail: String)
}
