package icfpc.classified.game

import icfpc.classified._

import WorldState._

case class WorldState(status: Status, attacker: Actor, defender: Actor, me: Actor) {
  def enemy: Actor = if (attacker == me) defender else attacker
}

object WorldState {
  sealed trait Status
  case object NotStarted extends Status
  case object Started extends Status
  case object Finished extends Status

  def parse(response: Expression): WorldState = {
    val responseList = response.toList
    val statusId = responseList.head.toLiteral
    val settings = responseList(2).toList
    val state = responseList(3).toList

    val status = statusId.value.toInt match {
      case 0 => NotStarted
      case 1 => Started
      case _ => Finished
    }

    val isDefence = settings(1).toLiteral.value.toInt == 1

    val actors = state(2).toList
    val defender = actors.head.toList
    val attacker = actors.last.toList

    val attackerCompiled = Actor(
      position = Vector(
        x = attacker.head.toList(2).toList.head.toLiteral.value.toLong,
        y = attacker.head.toList(2).toList.last.toLiteral.value.toLong
      ),
      speed = Vector(0, 0),
      health = 100
    )

    val defenderCompiled = Actor(
      position = Vector(
        x = defender.head.toList(2).toList.head.toLiteral.value.toLong,
        y = defender.head.toList(2).toList.last.toLiteral.value.toLong
      ),
      speed = Vector(0, 0),
      health = 100
    )

    WorldState(status, attackerCompiled, defenderCompiled, if (isDefence) defenderCompiled else attackerCompiled)
  }
}
