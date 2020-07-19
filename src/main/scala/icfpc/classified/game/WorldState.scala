package icfpc.classified.game

import icfpc.classified._
import WorldState._
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
    val attackerSpeed = attacker.head.toList(3).toPair
    val attackerCompiled = Actor(
      position = Vector(
        x = attackerPos._1.toLiteral.value.toInt,
        y = attackerPos._2.toLiteral.value.toInt
      ),
      speed = Vector(
        attackerSpeed._1.toLiteral.value.toInt,
        attackerSpeed._2.toLiteral.value.toInt
      ),
      health = 100
    )

    val defender = actors.head.toList
    val defenderPos = defender.head.toList(2).toPair
    val defenderSpeed = attacker.head.toList(3).toPair
    val defenderCompiled = Actor(
      position = Vector(
        x = defenderPos._1.toLiteral.value.toInt,
        y = defenderPos._2.toLiteral.value.toInt
      ),
      speed = Vector(
        defenderSpeed._1.toLiteral.value.toInt,
        defenderSpeed._2.toLiteral.value.toInt
      ),
      health = 100
    )

    WorldState(status, attackerCompiled, defenderCompiled, isDefence)
  }
}
