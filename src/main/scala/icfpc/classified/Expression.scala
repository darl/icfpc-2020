package icfpc.classified

sealed trait Expression {
  def asExpression0: Expression0 = throw new IllegalArgumentException(s"Can't convert $this to Expression0")
  def asExpression1: Expression1 = throw new IllegalArgumentException(s"Can't convert $this to Expression1")
  def asVariable: Variable = throw new IllegalArgumentException(s"Can't convert $this to Variable")
}

sealed trait Expression0 extends Expression {
  def eval: Expression

  override def asExpression0: Expression0 = this

  override def asVariable: Variable = this.eval.asVariable
}

sealed trait Expression1 extends Expression {
  def eval(x: Expression): Expression

  override def asExpression1: Expression1 = this
}

case class Variable(value: Int) extends Expression0 {
  override def eval: Expression = this

  override def asExpression0: Expression0 = this

  override def asVariable: Variable = this

}

case class Apply(op: Expression, arg: Expression) extends Expression0 {

  def eval: Expression = {
    (op, arg) match {
      case (operation: Expression1, arg: Expression0) => operation.eval(arg.eval)
      case (expr1, expr2) => throw new IllegalArgumentException(s"Can't eval apply on $expr1, $expr2")
    }
  }
}

case class Sum(left: Expression) extends Expression1 {
  override def eval(right: Expression): Expression = Variable(left.asVariable.value + right.asVariable.value)
}
