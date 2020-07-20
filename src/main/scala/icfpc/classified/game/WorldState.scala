package icfpc.classified.game

import icfpc.classified.game.WorldState._
import icfpc.classified.replay.Capture
import icfpc.classified.syntax.Expression

case class WorldState(
    status: Status,
    attackers: List[Actor],
    defenders: List[Actor],
    isDefence: Boolean,
    moveNumber: Int,
    debug: Option[Any] = None) {
  def me: Actor = if (isDefence) defenders.head else attackers.head

  def nearestEnemy: Actor =
    enemies.minBy(e => (e.position - me.position).length)

  def strongestEnemy: Actor =
    enemies.maxBy(_.stats.might)

  def enemies: List[Actor] = if (isDefence) attackers else defenders

  def myAdds: List[Actor] =
    if (isDefence) defenders.tail
    else attackers.tail

  def center: Vector = Vector(0, 0)

  def blackBox: Array[Vector] =
    Array(
      Vector(14, 14),
      Vector(14, -14),
      Vector(-14, -14),
      Vector(-14, 14)
    )
}

object WorldState {
  sealed trait Status
  case object NotStarted extends Status
  case object Started extends Status
  case object Finished extends Status

  def parse(response: Expression)(implicit stateCapture: Capture[Expression]): WorldState = {
    stateCapture.log(response)
    val responseList = response.toList
    val statusId = responseList(1).toLiteral
    val settings = responseList(2).toList
    val state = responseList(3).toList

    val status = statusId.value.toInt match {
      case 0 => NotStarted
      case 1 => Started
      case _ => Finished
    }

    val isDefence = settings(1).toLiteral.value.toInt == 1

    val actors = state(2).toList.map(Actor.from)
    val defenders = actors.filter(_.isDefender)
    val attackers = actors.filterNot(_.isDefender)

    val moveNumber = state.head.toLiteral.value.toInt

    WorldState(status, attackers, defenders, isDefence, moveNumber, None)
  }
}
