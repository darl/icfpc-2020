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

  def normalize: Vector = widthLength(1)

  def round: Vector = {
    def round(v: Double): Double = {
      val s = v.sign
      val frac = v.abs - v.abs.floor
      if (frac >= 0.3d) {
        s * v.abs.ceil
      } else {
        s * v.abs.floor
      }
    }
    Vector(
      round(x),
      round(y)
    )
  }

  def isZero: Boolean = x == 0 && y == 0
  def nonZero: Boolean = !isZero
}

object Vector {
  val Zero: Vector = Vector(0, 0)
}