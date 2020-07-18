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
}
