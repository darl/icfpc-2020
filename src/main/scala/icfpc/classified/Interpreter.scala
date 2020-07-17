package icfpc.classified

object Interpreter {

  def exec(expression: Expression): Literal = {
    expression match {
      case app: Apply =>
        operation(app.op)(Interpreter.exec(app.arg)) match {
          case result: Literal => result
          case _ => throw new IllegalStateException("Program result isn't literal")
        }
      case literal: Literal => literal
      case _: Op => throw new IllegalStateException("Can't exec Op")
    }
  }

  private def operation(expression: Expression): Literal => Expression = {
    expression match {
      case op: Op => execOp(op)
      case Apply(op, arg) =>
        operation(op)(Interpreter.exec(arg)) match {
          case carry: Op => arg => operation(carry)(arg)
          case _ => throw new IllegalStateException("Expecting op from apply to carry")
        }
      case _: Literal => throw new IllegalStateException("Can't operate with literal")
    }
  }

  private def execOp(op: Op): Literal => Expression = {
    op match {
      case UnknownVariable(value) => ???
      case Nil => ???
      case Cons0 => ???
      case Cons1(head) => ???
      case Cons(head, tail) => ???
      case False => ???
      case True => ???
      case Sum0 => Sum1.apply
      case Sum1(left) => arg => Literal(left.value + arg.value)
      case Product0 => ???
      case Product1(left) => arg => Literal(left.value * arg.value)
      case Negate0 => ???
      case Mul0 => ???
      case Mul1(left) => ???
      case Div0 => ???
      case Div1(left) => ???
      case Inc0 => ???
      case Dec0 => ???
      case Function(id, expr) => ???
    }
  }
}
