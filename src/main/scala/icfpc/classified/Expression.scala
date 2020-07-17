package icfpc.classified

sealed trait Expression

case class Literal(value: Int) extends Expression
case class UnknownVariable(value: Int) extends Expression

case class Apply(op: Expression, arg: Expression) extends Expression

case object Nil extends Expression
case class Cons(head: Expression, tail: Expression) extends Expression

case object False extends Expression
case object True extends Expression

case object Sum0 extends Expression
case class Sum1(left: Expression) extends Expression

case object Product0 extends Expression
case class Product1(left: Expression) extends Expression

//successor
case object Succ extends Expression
case object Pred extends Expression

case class Function(id: Int, expr: Expression) extends Expression
