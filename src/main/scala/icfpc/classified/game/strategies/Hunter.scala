package icfpc.classified.game.strategies

import icfpc.classified.game.Actor.Stats
import icfpc.classified.game.{Actions, Actor, WorldState}

object Hunter extends Strategy {

  def stats(isDefender: Boolean): Stats = Stats(0, 30, 16, 1)

  def hottestInRange(state: WorldState, value: Int): Option[Actor] = {
    val closest = state.enemies.filter { enemy =>
      (state.me.position - enemy.position).length < value
    }
    if (closest.isEmpty) {
      None
    } else {
      Some(closest.maxBy(_.heat))
    }
  }

  def selectEnemy(worldState: WorldState): Actor = {
    hottestInRange(worldState, 30).getOrElse(worldState.nearestEnemy)
  }

  def run(state: WorldState): Actions = {
    if (state.moveNumber == 0) {
      return Actions.empty
    }
    val me = state.me
    val strongest = state.strongestEnemy
    if (state.moveNumber < 6) {
      val targetSpeed = (state.me.position * -1).normal((strongest.speed * -1)).widthLength(7)
      return Actions.moveDirection(targetSpeed)
    }

    val enemy = selectEnemy(state)
    val distanceToEnemy = (state.me.position - enemy.position).length
    val move = if (state.me.trajectory.next(10).exists(_.isFatal) || distanceToEnemy < 30) {
      Default.move(state, enemy)
    } else {
      val enemyPosition = enemy.trajectory.next(5).toSeq.last.position
      val target = enemyPosition - me.position
      if (state.me.heat < 48 && target.length > 1)
        Actions.moveDirection(target - state.me.speed * 2)
      else Actions.empty
    }
    val fire = if (distanceToEnemy < 30) Default.fire(state, enemy) else Actions.empty

    move |+| fire
  }
}
