package icfpc.classified.game

case class WorldState(attacker: Actor, defender: Actor, me: Actor) {
  def enemy: Actor = if (attacker == me) defender else attacker
}
