package icfpc.classified

case class Interpreter(lib: Map[Long, Expression], sender: SignalSender) {

  def exec(expression: Expression): Literal = {
    eval(expression) match {
      case result: Literal => result
      case t =>
        if (t != expression) exec(t)
        else throw new IllegalStateException(s"Program result $t isn't literal")
    }
  }

  private[classified] def eval(expression: Expression): Expression = {
    expression match {
      case app: Apply => eval(operation(app.op)(app.arg))
      case literal: Literal => literal
      case op: Op => op
      case UnknownVariable(value) =>
        eval(lib.getOrElse(value, throw new IllegalStateException(s"Can't resolve unknown variable $value")))
    }
  }

  private def operation(expression: Expression): Expression => Expression = {
    eval(expression) match {
      case op: Op => execOp(op)
      case other => throw new IllegalStateException(s"Can't operate with $other")
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
      case SComb2(x0, x1) =>
        x2 => {
          val x2Eval = eval(x2)
          Apply(Apply(x0, x2Eval), Apply(x1, x2Eval))
        }

      case Car => arg => eval(arg).toCons.head
      case Cdr => arg => eval(arg).toCons.tail
      case Cons0 => Cons1.apply
      case Cons1(head) => arg => Cons(head, arg)
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

      case LessThan0 => arg => LessThan1(eval(arg).toLiteral)
      case LessThan1(left) => right => if (left.value < eval(right).toLiteral.value) True0 else False0
      case EqualTo0 => arg => EqualTo1(eval(arg).toLiteral)
      case EqualTo1(left) => right => if (left.value == eval(right).toLiteral.value) True0 else False0
      case IfZero0 => arg => IfZero1(eval(arg).toLiteral.value == 0)
      case IfZero1(cond) => left => IfZero2(cond, left)
      case IfZero2(cond, left) => right => if (cond) left else right
      case Identity => identity

      case Send =>
        arg =>
          val signal = Modulator.modulate(arg)
          val result = sender.send(signal)
          Demodulator.demodulate(result)
      case Interact0 => arg => Interact1(arg)
      case Interact1(protocol) => arg => Interact2(protocol, arg)
      case Interact2(protocol, state) => vector => makeList(0, vector)
    }
  }

  implicit class RichExpression(private val expression: Expression) {
    def apply(arg: Expression): Apply = Apply(expression, arg)

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
