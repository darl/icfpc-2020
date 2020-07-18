package icfpc

import scala.language.implicitConversions

package object classified {
  implicit def literal(value: Int): Literal = Literal(value)
  implicit def literal(value: Long): Literal = Literal(value)
  implicit def literal(value: BigInt): Literal = Literal(value)

  def makeList(elems: Expression*): Expression = {
    var elements = elems.reverse.toList
    var res: Expression = Nil
    while (elements.nonEmpty) {
      res = Cons(elements.head, res)
      elements = elements.tail
    }
    res
  }

  def pair(a: Expression, b: Expression): Cons = {
    Cons(a, b)
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
