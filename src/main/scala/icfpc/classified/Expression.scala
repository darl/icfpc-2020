package icfpc.classified

sealed trait Expression
sealed trait Basic extends Expression

sealed trait Op extends Expression with Basic
case class Apply(op: Expression, arg: Expression) extends Expression with Basic

case class Literal(value: Int) extends Expression with Basic
case class UnknownVariable(value: Int) extends Op with Basic

case object Nil extends Expression with Basic with Op

case object Cons0 extends Expression with Basic with Op
case class Cons1(head: Expression) extends Expression with Op
case class Cons(head: Expression, tail: Expression) extends Expression with Op

case object False extends Expression with Basic with Op
case object True extends Expression with Basic with Op

case object Sum0 extends Expression with Basic with Op
case class Sum1(left: Expression) extends Expression with Op

case object Product0 extends Expression with Basic with Op
case class Product1(left: Expression) extends Expression with Op

case object Negate0 extends Expression with Basic with Op

case object Mul0 extends Expression with Basic with Op
case class Mul1(left: Expression) extends Expression with Op

case object Div0 extends Expression with Basic with Op
case class Div1(left: Expression) extends Expression with Op

//increment
case object Inc0 extends Expression with Basic with Op
case object Dec0 extends Expression with Basic with Op

case class Function(id: Int, expr: Expression) extends Op
