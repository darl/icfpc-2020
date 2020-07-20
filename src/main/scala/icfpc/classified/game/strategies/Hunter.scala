package icfpc.classified.game.strategies

import icfpc.classified.game.Actor.Stats
import icfpc.classified.game.{Actions, Vector, WorldState}

object Hunter extends Strategy {

  def stats(isDefender: Boolean): Stats = Stats(0, 30, 16, 1)

  def run(state: WorldState): Actions = {
    val me = state.me
    val enemy = (state.enemy :: state.enemyAdds).minBy(e => (e.position - me.position).length)

    val distanceToEnemy = (state.me.position - state.enemy.position).length
    if (state.me.trajectory.next(10).exists(_.isFatal) || distanceToEnemy < 30) return Default.run(state)
    val enemyPosition = enemy.trajectory.next(5).toSeq.last.position
    val target = enemyPosition - me.position
    val move =
      if (state.me.heat < 48 && target.length > 1)
        Actions.moveDirection(target - state.me.speed * 2)
      else Actions.empty

    val fire =
      if (state.me.heat <= 32 && distanceToEnemy < 30) {
        val g = state.enemy.position.normalize.!
        val fireDirection = state.enemy.position + state.enemy.speed + g
        Actions.fire(fireDirection.round, state.me.maxFirePower)
      } else Actions.empty

    move |+| fire
  }

}
