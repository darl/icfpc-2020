package icfpc

package object classified {
  implicit def literal(value: Int): Literal = Literal(value)
}
