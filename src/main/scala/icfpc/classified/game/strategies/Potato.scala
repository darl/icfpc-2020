package icfpc.classified.game.strategies

import icfpc.classified.game.{Actions, Actor, Vector, WorldState}

object Potato extends Strategy {
  override def stats(isDefender: Boolean): Actor.Stats = Actor.Stats(0, 0, 16, 1)

  override def run(state: WorldState): Actions = {
    val vector = if (state.me.position.x > state.me.position.y) {
      if (-state.me.position.x > state.me.position.y) {
        Vector(0, 1)
      } else {
        Vector(-1, 0)
      }
    } else {
      if (-state.me.position.x > state.me.position.y) {
        Vector(1, 0)
      } else {
        Vector(0, -1)
      }
    }
    Actions(drive = Some(Actions.Drive(vector.x.toInt, vector.y.toInt)))
  }
}
