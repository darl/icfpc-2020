package icfpc.classified.syntax

object Parser {

  private val basicOperations: Seq[(String, Ast, Int)] = List(
    ("inc", Inc0, 0),
    ("dec", Dec0, 0),
    ("add", Sum0, 0),
    ("mul", Mul0, 0),
    ("div", Div0, 0),
    ("eq", EqualTo0, 0),
    ("lt", LessThan0, 0),
    ("neg", Negate0, 0),
    ("s", SComb0, 0),
    ("c", CComb0, 0),
    ("b", BComb0, 0),
    ("pwr2", Power2, 0),
    ("i", Identity, 0),
    ("t", True0, 0),
    ("f", False0, 0),
    ("nil", Nil, 0),
    ("cons", Cons0, 0),
    ("vec", Cons0, 0),
    ("car", Car, 0),
    ("cdr", Cdr, 0),
    ("isnil", IsNil, 0),
    ("draw", Draw, 0),
    ("multipledraw", MultiDraw, 0),
    ("checkerboard", Checkerboard0, 0),
    ("send", Send, 0),
    ("if0", IfZero0, 0),
    ("interact", Interact0, 0)
  )

  def parseBinary(data: List[Int]): Expression = ???

  def parseText(data: List[String]): (Ast, List[String]) = {
    data.head match {
      case "ap" =>
        val (op, rest) = parseText(data.tail)
        val (arg, tail) = parseText(rest)
        Apply(op, arg) -> tail

      //todo list syntax
      case v =>
        basicOperations.find(_._1 == v) match {
          case Some(value) =>
            value._2 -> data.tail
          case None =>
            if (v.startsWith(":")) UnknownVariable(v.stripPrefix(":").toLong) -> data.tail
            else Literal(v.toLong) -> data.tail
        }
    }
  }

}
