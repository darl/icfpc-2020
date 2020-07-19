package icfpc.classified.game

import icfpc.classified.game.Actor.Stats
import icfpc.classified.syntax._

case class Actor(
    health: Int,
    position: Vector,
    speed: Vector,
    stats: Stats,
    heat: Int,
    x6: Int,
    x7: Int,
    performedActions: Seq[PerformedAction]) {

  lazy val trajectory: Trajectory = new Trajectory(position, speed)
}

object Actor {

  case class Stats(supply: Int, x: Int, cooling: Int, z: Int)

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
      x7 = actorState(7).toLiteral.value.toInt,
      performedActions = parsePerformedActions(actor(1))
    )
  }

  def parsePerformedActions(expr: Expression): Seq[PerformedAction] = {
    val wrapped = expr match {
      case Cons(Literal(_), _) => List(expr)
      case other => other.toList
    }

    val data = wrapped.map(_.toList)
    val fired = data.find(_.head == Literal(2)).map { a =>
      val pair = a(1).toPair
      Fired(
        Vector(pair._1.toLiteral.value.toInt, pair._2.toLiteral.value.toInt),
        a(2).toLiteral.value.toInt,
        a(3).toLiteral.value.toInt,
        a(4).toLiteral.value.toInt
      )
    }

    val drove = data.find(_.head == Literal(0)).map { m =>
      val pair = m(1).toPair
      Drove(pair._1.toLiteral.value.toInt, pair._2.toLiteral.value.toInt)
    }

    List(fired, drove).flatten
  }
}
