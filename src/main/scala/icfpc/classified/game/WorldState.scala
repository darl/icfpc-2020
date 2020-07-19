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
    val attacker = actors.last.toList
    val attackerPos = attacker.head.toList(2).toPair
    val attackerCompiled = Actor(
      position = Vector(
        x = attackerPos._1.toLiteral.value.toLong,
        y = attackerPos._2.toLiteral.value.toLong
      ),
      speed = Vector(0, 0),
      health = 100
    )

    val defender = actors.head.toList
    val defenderPos = defender.head.toList(2).toPair
    val defenderCompiled = Actor(
      position = Vector(
        x = defenderPos._1.toLiteral.value.toLong,
        y = defenderPos._2.toLiteral.value.toLong
      ),
      speed = Vector(0, 0),
      health = 100
    )

    WorldState(status, attackerCompiled, defenderCompiled, if (isDefence) defenderCompiled else attackerCompiled)
  }
}
