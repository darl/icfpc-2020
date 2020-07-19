package icfpc.classified.game

case class Vector(x: Int, y: Int) {
  def +(other: Vector): Vector = Vector(x + other.x, y + other.y)

  def *(v: Double) = Vector((x * v).toInt, (y * v).toInt)

  def norm: Vector =
    Vector(
      x.max(-1).min(1),
      y.max(-1).min(1)
    )

  def isZero: Boolean = x == 0 && y == 0
  def nonZero: Boolean = !isZero
}
