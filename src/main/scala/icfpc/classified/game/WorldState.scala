package icfpc.classified.game

import icfpc.classified.game.WorldState._
import icfpc.classified.replay.Capture
import icfpc.classified.syntax.Expression

case class WorldState(
    status: Status,
    attacker: Actor,
    defender: Actor,
    adds: List[Actor],
    isDefence: Boolean,
    moveNumber: Int,
    debug: Option[Any] = None) {
  def me: Actor = if (isDefence) defender else attacker
  def enemy: Actor = if (isDefence) attacker else defender

  def myAdds: List[Actor] =
    if (isDefence) adds.filter(_.isDefender)
    else adds.filterNot(_.isDefender)

  def enemyAdds: List[Actor] =
    if (isDefence) adds.filterNot(_.isDefender)
    else adds.filter(_.isDefender)

  def center: Vector = Vector(0, 0)

  def blackBox: Array[Vector] =
    Array(
      Vector(14, 14),
      Vector(14, -14),
      Vector(-14, -14),
      Vector(-14, 14)
    )

  def nearestEnemy: Actor = (enemy :: enemyAdds).minBy(e => (e.position - me.position).length)
  def strongestEnemy: Actor = (enemy :: enemyAdds).maxBy(_.stats.sum)
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
    val defender = actors.filter(_.isDefender).head
    val attacker = actors.filterNot(_.isDefender).head
    val adds = actors.drop(2)

    val moveNumber = state.head.toLiteral.value.toInt

    val debug = (attacker.position - defender.position).length
    WorldState(status, attacker, defender, adds, isDefence, moveNumber, debug = Some(debug))
  }
}
