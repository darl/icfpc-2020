package icfpc.classified.game

case class Vector(x: Long, y: Long) {
  def +(other: Vector): Vector = Vector(x + other.x, y + other.y)
}
