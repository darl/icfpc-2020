package icfpc.classified

sealed trait Expression
sealed trait Ast extends Expression

sealed trait Op extends Expression

case class Apply(op: Expression, arg: Expression) extends Expression with Ast {
  override def toString: String = s"Apply($op, $arg)"
}

case class Literal(value: Long) extends Ast
case class UnknownVariable(value: Long) extends Ast

case object Nil extends Expression with Ast with Op

case object Cons0 extends Expression with Ast with Op
case class Cons1(head: Expression) extends Op
case class Cons(head: Expression, tail: Expression) extends Op

case object False0 extends Expression with Ast with Op
case object False extends Expression with Op
case object True0 extends Expression with Ast with Op
case class True(value: Expression) extends Op

case object Sum0 extends Expression with Ast with Op
case class Sum1(left: Literal) extends Op

case object Negate0 extends Expression with Ast with Op

case object Mul0 extends Expression with Ast with Op
case class Mul1(left: Literal) extends Op

case object Div0 extends Expression with Ast with Op
case class Div1(left: Literal) extends Op

case object EqualTo0 extends Op with Ast
case class EqualTo1(left: Literal) extends Op

case object LessThan0 extends Op with Ast
case class LessThan1(left: Literal) extends Op

//increment
case object Inc0 extends Expression with Ast with Op
case object Dec0 extends Expression with Ast with Op

case object SComb0 extends Ast with Op
case class SComb1(a: Expression) extends Op
case class SComb2(a: Expression, b: Expression) extends Op

case object CComb0 extends Ast with Op
case class CComb1(a: Expression) extends Op
case class CComb2(a: Expression, b: Expression) extends Op

case object BComb0 extends Ast with Op
case class BComb1(a: Expression) extends Op
case class BComb2(a: Expression, b: Expression) extends Op

case object Power2 extends Ast with Op

case object Identity extends Ast with Op

case object Car extends Ast with Op
case object Cdr extends Ast with Op

case object IsNil extends Ast with Op

case object Draw extends Ast with Op
case object MultiDraw extends Ast with Op

case object Checkerboard0 extends Ast with Op
case class Checkerboard1(width: Expression) extends Op

case object Send extends Ast with Op

case object IfZero0 extends Ast with Op
case class IfZero1(cond: Boolean) extends Op
case class IfZero2(cond: Boolean, left: Expression) extends Op

case object Interact0 extends Ast with Op
case class Interact1(protocol: Expression) extends Ast with Op
case class Interact2(protocol: Expression, state: Expression) extends Ast with Op

case class FunctionDef(id: Long, expr: Expression) extends Ast
