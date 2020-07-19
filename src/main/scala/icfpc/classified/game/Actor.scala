package icfpc.classified.game

import icfpc.classified.syntax._

case class Actor(position: Vector, speed: Vector, health: Int)

object Actor {

  def from(exception: Expression): Actor = {
    val actor = exception.toList
    val pos = actor.head.toList(2).toPair
    val speed = actor.head.toList(3).toPair
    Actor(
      position = Vector(
        x = pos._1.toLiteral.value.toInt,
        y = pos._2.toLiteral.value.toInt
      ),
      speed = Vector(
        speed._1.toLiteral.value.toInt,
        speed._2.toLiteral.value.toInt
      ),
      health = actor.head.toList(4).toList.head.toLiteral.value.toInt
    )
  }
}
