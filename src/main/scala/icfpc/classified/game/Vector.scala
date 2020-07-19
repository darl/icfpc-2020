package icfpc.classified.game

case class Vector(x: Long, y: Long) {
  def +(other: Vector): Vector = Vector(x + other.x, y + other.y)

  def *(v: Double) = Vector((x * v).toLong, (y * v).toLong)

  def norm: Vector =
    Vector(
      x.max(-1L).min(1L),
      y.max(-1L).min(1L)
    )
}
