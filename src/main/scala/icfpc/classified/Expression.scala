package icfpc.classified

sealed trait Expression

case class Literal(value: Int) extends Expression
case class UnknownVariable(value: Int) extends Expression

case class Apply(op: Expression, arg: Expression) extends Expression

case object Nil extends Expression

case class Cons(head: Expression, tail: Expression) extends Expression

case object False extends Expression
case object True extends Expression

case class Sum(left: Expression, right: Expression) extends Expression

case class Product(left: Expression, right: Expression) extends Expression

case class Negate(exp: Expression) extends Expression

case class Div(left: Expression, right: Expression) extends Expression

//successor
case class Succ(exp: Expression) extends Expression
case class Pred(exp: Expression) extends Expression

case class Function(id: Int, expr: Expression) extends Expression
