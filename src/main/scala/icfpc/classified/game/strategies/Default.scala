package icfpc.classified.game.strategies

import icfpc.classified.game.Actor.Stats
import icfpc.classified.game.{Actions, WorldState}

object Default extends Strategy {

  def stats(isDefence: Boolean): Stats = {
    Stats(90, if(isDefence) 40 else 54, 14, 7)
  }

  def run(state: WorldState): Actions = {
    val targetSpeed = state.me.position.normal.widthLength(7)
    val targetForce = targetSpeed - state.me.speed
    val move =
      if (state.me.heat < 48 && targetForce.length > 3) Actions.moveDirection(targetForce)
      else Actions.empty

    val fire =
      if (state.me.heat <= 24) {
        val my = state.me.trajectory.next(8)
        val enemy = state.enemy.trajectory.next(8)

        val future = my.zip(enemy).zipWithIndex.map {
          case ((me, enemy), idx) => (me.position - enemy.position).length -> idx
        }

        val min = future.toVector.sortBy(_._1)
        if (min.take(2).exists(_._2 == 0)) {
          val fireDirection = state.enemy.trajectory.next.position
          Actions.fire(fireDirection.round)
        } else {
          Actions.empty
        }
      } else Actions.empty

    move |+| fire
  }
}
