package icfpc.classified

class Parser {
  def parseBinary(data: Iterator[Int]): Expression = ???

  def parseText(data: Iterator[String]): Expression = ???

  def printText(expression: Expression): String = {
    expression match {
      case Literal(value) => value.toString
      case UnknownVariable(value) => s":$value"
      case Apply(op, arg) => s"ap ${printText(op)} ${printText(arg)}"
      case Nil => "nil"
      case Cons0 => "cons"
      case Cons1(tail) => printText(Apply(Cons0, tail))
      case Cons(head, tail) => printText(Apply(Cons1(head), tail))
      case Div(left, right) => s"ap ap div ${printText(left)} ${printText(right)}"
    }
  }
}
