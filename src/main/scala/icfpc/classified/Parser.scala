package icfpc.classified

class Parser {

  val basic: Seq[(String, Basic, Int)] = List(
    ("inc", Inc0, 0),
    ("dec", Dec0, 0),
    ("mul", Mul0, 0),
    ("div", Div0, 0),
    ("nil", Nil, 0),
    ("cons", Cons0, 0)
  )

  def parseBinary(data: List[Int]): Expression = ???

  def parseText(data: List[String]): (Expression with Basic, List[String]) = {
    data.head match {
      case "ap" =>
        val (op, rest) = parseText(data.tail)
        val (arg, tail) = parseText(rest)
        Apply(op, arg) -> tail
      case v =>
        basic.find(_._1 == v) match {
          case Some(value) =>
            value._2 -> data.tail
          case None =>
            if (v.startsWith(":")) UnknownVariable(v.stripPrefix(":").toInt) -> data.tail
            else Literal(v.toInt) -> data.tail
        }
    }
  }

  def printText(expression: Expression): String = {
    toBasic(expression) match {
      case Literal(value) => value.toString
      case UnknownVariable(value) => s":$value"
      case Apply(op, arg) => s"ap ${printText(op)} ${printText(arg)}"
      case v =>
        basic.find(_._2 == v) match {
          case Some(value) => value._1
          case None => s"unknown($v)"
        }
    }
  }

  def toBasic(expression: Expression): Basic = {
    expression match {
      case b: Basic => b
      case Cons1(head) => Apply(Cons0, head)
      case Cons(head, tail) => Apply(Cons1(head), tail)
      case Sum1(left) => Apply(Sum0, left)
      case Product1(left) => Apply(Product0, left)
      case Mul1(left) => Apply(Mul0, left)
      case Div1(left) => Apply(Div0, left)
      case Function(id, expr) => ???
    }
  }
}
