package icfpc.classified

import scala.annotation.tailrec
import scala.collection.immutable.{List => ScalaList}
import scala.collection.mutable

object Modulator {

  def modulate(expression: Expression): String = {
    val stack = mutable.Stack(expression)
    val builder = new mutable.StringBuilder()
    while (stack.nonEmpty) {
      stack.pop match {
        case Nil => builder.append("00")
        case Literal(value) => builder.append(modulateLiteral(value))
        case Cons(head, tail) =>
          builder.append("11")
          stack.push(tail)
          stack.push(head)
        case _ => throw new IllegalArgumentException("Can modulate only literals and lists of lists and literals")
      }
    }
    builder.toString()
  }

  private def modulateLiteral(value: Int): String = {
    val sign = if (value >= 0) "01" else "10"

    val binaryValue = java.lang.Long.toString(math.abs(value), 2)
    val addZeros =
      if (binaryValue.length % 4 == 0) 0
      else 4 - binaryValue.length % 4
    val encodedValue =
      if (value == 0) ""
      else 1.to(addZeros).map(_ => 0).mkString + binaryValue
    val encodedWidth =
      if (value == 0) ""
      else 1.to(encodedValue.length / 4).map(_ => 1).mkString

    sign + encodedWidth + "0" + encodedValue
  }
}
