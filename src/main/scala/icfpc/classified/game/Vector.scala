package icfpc.classified.game

case class Vector(x: Double, y: Double) {
  def +(other: Vector): Vector = Vector(x + other.x, y + other.y)
  def -(other: Vector): Vector = Vector(x - other.x, y - other.y)

  def *(v: Double): Vector = Vector(x * v, y * v)
  def /(v: Double): Vector = Vector(x / v, y / v)

  def |*|(other: Vector): Double = x * other.x + y * other.y

  def ! : Vector = this * -1

  def length: Double = math.sqrt(x * x + y * y)

  def map(f: Double => Double): Vector = Vector(f(x), f(y))

  def widthLength(v: Double): Vector = {
    val len = length
    Vector((x * v / len), (y * v / len))
  }

  def normal(other: Vector): Vector = {
    if (other.x != 0) {
      val nx = other.x
      val ny = (-x * nx) / y
      Vector(nx, ny)
    } else {
      val ny = other.y
      val nx = (-y * ny) / x
      Vector(nx, ny)
    }
  }

  def normalize: Vector = widthLength(1)

  def round: Vector = {
    Vector(
      x.round,
      y.round
    )
  }

  def angleTo(another: Vector): Double = math.acos((this |*| another) / this.length / another.length) / Math.PI * 180

  def isZero: Boolean = x == 0 && y == 0
  def nonZero: Boolean = !isZero
}

object Vector {
  val Zero: Vector = Vector(0, 0)
}
