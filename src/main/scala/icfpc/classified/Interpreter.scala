package icfpc.classified

object Interpreter {

  def exec(expression: Expression): Literal = {
    eval(expression) match {
      case result: Literal => result
      case apply: Apply => exec(apply)
      case t =>
        throw new IllegalStateException(s"Program result $t isn't literal")
    }
  }

  private def eval(expression: Expression): Expression = {
    expression match {
      case app: Apply => operation(app.op)(app.arg)
      case literal: Literal => literal
      case op: Op => op
    }
  }

  private def operation(expression: Expression): Expression => Expression = {
    expression match {
      case Apply(op, arg) =>
        operation(op)(arg) match {
          case carry: Op => arg => operation(carry)(arg)
          case _ => throw new IllegalStateException("Expecting op from apply to carry")
        }
      case op: Op => execOp(op)
      case _: Literal => throw new IllegalStateException("Can't operate with literal")
    }
  }

  private def execOp(op: Op): Expression => Expression = {

    op match {
      //ap ap ap b x0 x1 x2   =   ap x0 ap x1 x2
      case BComb0 => BComb1.apply
      case BComb1(x0) => x1 => BComb2(x0, x1)
      case BComb2(x0, x1) => x2 => Apply(x0, Apply(x1, x2))

      //ap ap ap c x0 x1 x2   =   ap ap x0 x2 x1
      case CComb0 => CComb1.apply
      case CComb1(x0) => x1 => CComb2(x0, x1)
      case CComb2(x0, x1) => x2 => Apply(Apply(x0, x2), x1)

      //ap ap ap s x0 x1 x2   =   ap ap x0 x2 ap x1 x2
      case SComb0 => SComb1.apply
      case SComb1(x0) => x1 => SComb2(x0, x1)
      case SComb2(x0, x1) => x2 => Apply(Apply(x0, x2), Apply(x1, x2))

      case Car => arg => eval(arg).toCons.head
      case Cdr => arg => eval(arg).toCons.tail
      case Cons0 => Cons1.apply
      case Cons1(tail) => arg => Cons(arg, tail)
      case cons: Cons => arg => Cons(arg, cons)
      case Nil => _ => True0
      case IsNil => arg => if (eval(arg) == Nil) True0 else False0
      case True(value) => _ => value
      case True0 => True.apply

      case False => identity
      case False0 => _ => False

      case Dec0 => arg => Literal(eval(arg).toLiteral.value - 1)
      case Inc0 => arg => Literal(eval(arg).toLiteral.value + 1)
      case Div0 => arg => Div1.apply(eval(arg).toLiteral)
      case Div1(left) => right => Literal(left.value / eval(right).toLiteral.value)
      case Mul0 => arg => Mul1.apply(eval(arg).toLiteral)
      case Mul1(left) => right => Literal(left.value * eval(right).toLiteral.value)
      case Sum0 => arg => Sum1.apply(eval(arg).toLiteral)
      case Sum1(left) => arg => Literal(left.value + eval(arg).toLiteral.value)
      case Negate0 => arg => Literal(-eval(arg).toLiteral.value)
      case Power2 => arg => Literal(Math.pow(2, eval(arg).toLiteral.value).toInt)

      case LessThan0 => ???
      case LessThan1(left) => ???
      case EqualTo0 => ???
      case EqualTo1(left) => ???
      case Identity => ???

      case UnknownVariable(value) => ???
    }
  }

  implicit class RichExpression(val expression: Expression) {

    def toLiteral: Literal =
      expression match {
        case res: Literal => res
        case other => throw new IllegalStateException(s"Can't convert $other to Literal")
      }

    def toCons: Cons =
      expression match {
        case res: Cons => res
        case other => throw new IllegalStateException(s"Can't convert $other to Cons")
      }
  }
}
