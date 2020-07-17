package icfpc.classified

sealed trait Expression

case class Literal(value: Int) extends Expression

case class Apply(op: Expression, arg: Expression) extends Expression

case object Nil extends Expression
case class Cons(head: Expression, tail: Expression) extends Expression

case object False extends Expression
case object True extends Expression

case object Sum0 extends Expression

case class Sum(left: Expression) extends Expression

case class Function(name: String, expr: Expression) extends Expression
