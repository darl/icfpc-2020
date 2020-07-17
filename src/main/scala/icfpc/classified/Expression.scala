package icfpc.classified

sealed trait Expression

sealed trait Op extends Expression
case class Apply(op: Expression, arg: Expression) extends Expression
case class Literal(value: Int) extends Expression

case class UnknownVariable(value: Int) extends Op

case object Nil extends Op

case object Cons0 extends Op
case class Cons1(head: Expression) extends Op
case class Cons(head: Expression, tail: Expression) extends Op

case object False extends Op
case object True extends Op

case object Sum0 extends Op
case class Sum1(left: Literal) extends Op

case object Product0 extends Op
case class Product1(left: Expression) extends Op
case class Product(left: Expression, right: Expression) extends Op

case object Negate0 extends Op
case class Negate(exp: Expression) extends Op

case object Div0 extends Op
case class Div1(left: Expression) extends Op
case class Div(left: Expression, right: Expression) extends Op

//successor
case object Succ0 extends Op
case class Succ(exp: Expression) extends Op
case object Pred0 extends Op
case class Pred(exp: Expression) extends Op

case class Function(id: Int, expr: Expression) extends Op
