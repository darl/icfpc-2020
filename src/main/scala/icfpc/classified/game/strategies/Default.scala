package icfpc.classified.game.strategies

import icfpc.classified.game.Actor.Stats
import icfpc.classified.game.{Actions, WorldState}

object Default extends Strategy {

  def stats: Stats = Stats(254, 10, 8, 1)

  def run(state: WorldState): Actions = {
    val targetSpeed = state.me.position.normal.widthLength(7)
    val targetForce = targetSpeed - state.me.speed
    val move =
      if (state.me.heat < 48 && targetForce.length > 2) Actions.moveDirection(targetForce)
      else Actions.empty

    val fire =
      if (state.me.heat <= 32) {
        val g = state.enemy.position.normalize.!
        val fireDirection = state.enemy.position + state.enemy.speed + g
        Actions.fire(fireDirection.round)
      } else Actions.empty

    move |+| fire
  }

}
