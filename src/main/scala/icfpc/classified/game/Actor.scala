package icfpc.classified.game

import icfpc.classified.game.Actor.Stats
import icfpc.classified.syntax._

case class Actor(health: Int, position: Vector, speed: Vector, stats: Stats, heat: Int, x6: Int, x7: Int)

object Actor {

  case class Stats(supply: Int, x: Int, y: Int, z: Int)

  def from(exception: Expression): Actor = {
    val actor = exception.toList
    val actorState = actor.head.toList
    val pos = actorState(2).toPair
    val speed = actorState(3).toPair
    val stats = actorState(4).toList
    Actor(
      health = 100,
      position = Vector(
        x = pos._1.toLiteral.value.toInt,
        y = pos._2.toLiteral.value.toInt
      ),
      speed = Vector(
        speed._1.toLiteral.value.toInt,
        speed._2.toLiteral.value.toInt
      ),
      stats = Stats(
        stats(0).toLiteral.value.toInt,
        stats(1).toLiteral.value.toInt,
        stats(2).toLiteral.value.toInt,
        stats(3).toLiteral.value.toInt
      ),
      heat = actorState(5).toLiteral.value.toInt,
      x6 = actorState(6).toLiteral.value.toInt,
      x7 = actorState(7).toLiteral.value.toInt
    )
  }
}
