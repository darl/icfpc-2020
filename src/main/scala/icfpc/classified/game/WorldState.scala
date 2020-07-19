package icfpc.classified.game

import icfpc.classified._
import WorldState._
import icfpc.classified.replay.StateCapture
import icfpc.classified.syntax.Expression

case class WorldState(status: Status, attacker: Actor, defender: Actor, isDefence: Boolean) {
  def me: Actor = if (isDefence) defender else attacker
  def enemy: Actor = if (isDefence) attacker else defender
}

object WorldState {
  sealed trait Status
  case object NotStarted extends Status
  case object Started extends Status
  case object Finished extends Status

  def parse(response: Expression)(implicit stateCapture: StateCapture): WorldState = {
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

    val actors = state(2).toList
    val defender = Actor.from(actors.head)
    val attacker = Actor.from(actors.last)

    WorldState(status, attacker, defender, isDefence)
  }
}
