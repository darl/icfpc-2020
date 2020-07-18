package icfpc.classified

import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable

case class Interpreter(lib: Map[Long, Expression], sender: SignalSender) {
  val cache = new mutable.HashMap[Expression, Expression]()

  private val idGen = new AtomicLong()

  def exec(expression: Expression): Literal = {
    eval(expression) match {
      case result: Literal => result
      case t =>
        if (t != expression) exec(t)
        else throw new IllegalStateException(s"Program result $t isn't literal")
    }
  }

  def eval(expression: Expression): Expression = {
    val cached = cache.get(expression)
    if (cached.nonEmpty) return cached.get

    @scala.annotation.tailrec
    def doEval(expression: Expression): Expression =
      expression match {
        case app: Apply => doEval(operation(app.op)(app.arg))
        case UnknownVariable(value) =>
          lib.getOrElse(value, throw new IllegalStateException(s"Can't resolve unknown variable $value"))
        case e => e
      }

//    val id = idGen.incrementAndGet()
//    println(s"Eval($id) = " + expression)
    var prevResult = expression
    var result = doEval(expression)
//    println(s"Result($id) = " + result)
    while (result != prevResult) {
      prevResult = result
      result = doEval(result)
//      println(s"Result($id) = " + result)
    }
    if (expression.canCache) cache.put(expression, result)
    result
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
      case CComb0 => arg => CComb1(arg)
      case CComb1(x0) => x1 => CComb2(x0, x1)
      case CComb2(x0, x1) => x2 => x0(x2)(x1)

      //ap ap ap s x0 x1 x2   =   ap ap x0 x2 ap x1 x2
      case SComb0 => SComb1.apply
      case SComb1(x0) => x1 => SComb2(x0, x1)
      case SComb2(x0, x1) =>
        x2 =>
          val xx2 = eval(x2)
          Apply(Apply(x0, xx2), Apply(x1, xx2))

      case Car => arg => eval(arg).toCons.head
      case Cdr => arg => eval(arg).toCons.tail
      case Cons0 => arg => Cons1(arg)
      case Cons1(head) => arg => Cons(eval(head), eval(arg))
      case Cons(head, tail) => arg => Apply(Apply(eval(arg), head), tail)
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
      case Power2 => arg => Literal(Math.pow(2, eval(arg).toLiteral.value.toLong).toInt)

      case LessThan0 => arg => LessThan1(eval(arg).toLiteral)
      case LessThan1(left) => right => if (left.value < eval(right).toLiteral.value) True0 else False0
      case EqualTo0 => arg => EqualTo1(eval(arg).toLiteral)
      case EqualTo1(left) => right => if (left.value == eval(right).toLiteral.value) True0 else False0
      case IfZero0 => arg => IfZero1(eval(arg).toLiteral.value == 0)
      case IfZero1(cond) => left => IfZero2(cond, left)
      case IfZero2(cond, left) => right => if (cond) left else right
      case Identity => identity

      case Draw => args => drawPoints(eval(args))
      case MultiDraw => args => multipleDraw(args)

      case Send => arg => send(eval(arg))
      case Interact0 => arg => Interact1(arg)
      case Interact1(protocol) => arg => Interact2(protocol, arg)
      case Interact2(protocol, state) => data => interact(eval(protocol), state, data)
    }
  }

  private def drawPoints(points: Expression): Canvas = {
    points match {
      case Nil => Canvas(List.empty)
      case Cons(Cons(Literal(x), Literal(y)), tail) =>
        drawPoints(tail).withPoint(x.toInt -> y.toInt)
      case other => throw new IllegalStateException(s"Can't convert $other to list of 2d points")
    }
  }

  // just to ensure that expression is serializable
  private def modem(ex: Expression): Expression = {
    val mod = Modulator.modulate(eval(ex))
    Demodulator.demodulate(mod)
  }

  private def multipleDraw(expression: Expression): Expression = {
    eval(expression) match {
      case Cons(head, tail) => Cons(eval(Apply(Draw, head)), eval(Apply(MultiDraw, tail)))
      case Nil => Nil
      case other => throw new IllegalStateException(s"Can't convert $other to list")
    }
  }

  private def send(data: Expression): Expression = {
    val signal = Modulator.modulate(eval(data))
//    println("Sending " + signal)
    val result = sender.send(signal)
//    println("Received " + result)
    Demodulator.demodulate(result)
  }

  private def f38(protocol: Expression, list: Expression): Expression = {
    val params = eval(list)
    val flag = params.toCons.head
    val tail1 = eval(params.toCons.tail)
    val newState = tail1.toCons.head
    val tail2 = eval(tail1.toCons.tail)
    val data = tail2.toCons.head

    if (flag == Literal(0))
      makeList(modem(newState), multipleDraw(data))
    else
      interact(protocol, modem(newState), send(data))
  }

  private def interact(protocol: Expression, state: Expression, data: Expression): Expression = {
    f38(protocol, protocol(state)(data))
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

    def toCanvas: Canvas =
      expression match {
        case c: Canvas => c
        case other => throw new IllegalStateException(s"Can't convert $other to Canvas")
      }

    def toPair: (Expression, Expression) =
      expression match {
        case res: Cons => res.head -> res.tail
        case other => throw new IllegalStateException(s"Can't convert $other to Cons")
      }

    def toList: List[Expression] = {
      expression match {
        case Nil => List.empty
        case Cons(head, tail) => head :: tail.toList
        case other => throw new IllegalStateException(s"Can't convert $other to List")
      }
    }

    def canCache: Boolean =
      expression match {
        case Apply(Send, _) => false
        case Apply(_: Interact2, _) => false
        case _ => true
      }
  }

}
