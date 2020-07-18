package icfpc.classified.game

import icfpc.classified._

case class WorldState(attacker: Actor, defender: Actor, me: Actor) {
  def enemy: Actor = if (attacker == me) defender else attacker
}

object WorldState {

  def parse(response: Expression): WorldState = {
    val responseList = response.toList
    val settings = response(2).toList
    val state = response(3).toList

    val actors = state(2).toList
    val defender = actors.head.toList
    val attacker = actors.last.toList

    val attackerCompiled = Actor(
      position = Vector(
        x = attacker.head.toList(2).toList.head.toLiteral.value.toLong,
        y = attacker.head.toList(2).toList.last.toLiteral.value.toLong
      ),
      speed = Vector(???, ???),
      health = 100
    )

    val defenderCompiled = Actor(
      position = Vector(
        x = defender.head.toList(2).toList.head.toLiteral.value.toLong,
        y = defender.head.toList(2).toList.last.toLiteral.value.toLong
      ),
      speed = Vector(???, ???),
      health = 100
    )

    WorldState(attackerCompiled, defenderCompiled, ???)
  }
}
