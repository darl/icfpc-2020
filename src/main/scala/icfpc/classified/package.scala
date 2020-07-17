package icfpc

package object classified {
  implicit def literal(value: Int): Variable = Variable(value)
}
