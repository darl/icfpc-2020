package icfpc.classified.game.strategies

import icfpc.classified.game.Actor.Stats
import icfpc.classified.game.{Actions, Vector, WorldState}

object Hunter extends Strategy {

  def stats(isDefender: Boolean): Stats = Stats(0, 30, 16, 1)

  def run(state: WorldState): Actions = {
    val enemy = state.enemy.position
    val me = state.me.position
    val target = enemy - me
    val td = target.normalize
    val nx = Math.abs(td.x)
    val ny = (-td.x * nx) / td.y
    val normal = Vector(nx, ny).normalize
    val factor = (5000 / me.length) * (1 - math.abs(normal |*| me.normalize))
    val result = (target + (normal * factor)) - state.me.speed.map(x => x * x)
    val move =
      if (state.me.heat < 48 && result.length > 1)
        Actions.moveDirection(result)
      else Actions.empty

    val fire =
      if (state.me.heat <= 32 && (state.me.position - state.enemy.position).length < 30) {
        val g = state.enemy.position.normalize.!
        val fireDirection = state.enemy.position + state.enemy.speed + g
        Actions.fire(fireDirection.round, state.me.maxFirePower)
      } else Actions.empty

    move |+| fire
  }

}
