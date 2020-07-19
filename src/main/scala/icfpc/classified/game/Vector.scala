package icfpc.classified.game

case class Vector(x: Double, y: Double) {
  def +(other: Vector): Vector = Vector(x + other.x, y + other.y)
  def -(other: Vector): Vector = Vector(x - other.x, y - other.y)

  def *(v: Double): Vector = Vector(x * v, y * v)
  def /(v: Double): Vector = Vector(x / v, y / v)

  def ! : Vector = this * -1

  def length: Double = math.sqrt(x * x + y * y)

  def widthLength(v: Double): Vector = {
    val len = length
    Vector((x * v / len), (y * v / len))
  }

  def normal: Vector =
    Vector(y, -x)

  def isZero: Boolean = x == 0 && y == 0
  def nonZero: Boolean = !isZero
}
