package icfpc.classified.game.strategies

import icfpc.classified.game.Actor.Stats
import icfpc.classified.game.{Actions, Actor, Vector, WorldState}

object Default extends Strategy {

  def stats(isDefence: Boolean): Stats = {
    Stats(90, if (isDefence) 49 else 63, 13, 1)
  }

  def run(state: WorldState): Actions = {
    val enemy = state.nearestEnemy
    val distanceToEnemy = (state.me.position - enemy.position).length

    val split =
      if (state.isDefence && state.moveNumber > 15) Actions.split(Stats(0, 0, 0, 1))
      else Actions.empty

    val addsActions = state.myAdds.zipWithIndex
      .map {
        case (add, idx) =>
          val detonate =
            if (distanceToEnemy < 6) Actions.detonate
            else Actions.empty

          detonate
      }
      .foldLeft(Actions.empty)(_ |+| _)

    move(state, enemy) |+| fire(state, enemy) |+| split |+| addsActions
  }

  def move(state: WorldState, enemy: Actor): Actions = {
    val distanceToEnemy = (state.me.position - enemy.position).length
    val nearEnemy = distanceToEnemy < 100 && enemy.canFire
    val conterClockwise = Vector(state.me.position.y, -state.me.position.x)
    val rotation = if (state.me.speed.isZero) conterClockwise else state.me.speed
    val targetSpeed = (state.me.position * -1).normal(rotation).widthLength(7)
    val targetForce = targetSpeed - state.me.speed
    if (state.me.canDrive && targetForce.length > 3 || state.me.trajectory.isFatalIn(16) || nearEnemy) {
      Actions.moveDirection(targetForce)
    } else Actions.empty
  }

  def fire(state: WorldState, enemy: Actor): Actions = {
    if (state.moveNumber > 1) {
      val perpDeviation = getFirePerpDeviation(state, enemy)
      val distanceToEnemy = (state.me.position - enemy.position).length

      if (state.me.canFire) {
        def isNearestPosition: Boolean = {
          val my = state.me.trajectory.next(8)
          val enemyTrajectory = enemy.trajectory.next(8)

          val future = my.zip(enemyTrajectory).zipWithIndex.map {
            case ((me, enemy), idx) => (me.position - enemy.position).length -> idx
          }

          val min = future.toVector.sortBy(_._1)
          min.take(2).exists(_._2 == 0)
        }

        if ((enemy.heat > 45 && distanceToEnemy < 100) || isNearestPosition || perpDeviation < 20) {
          val fireDirection = enemy.trajectory.next.position
          Actions.fire(fireDirection.round, state.me.maxFirePower)
        } else {
          Actions.empty
        }
      } else {
        if (enemy.heat > 45 && distanceToEnemy < 50 || perpDeviation < 10) {
          Actions.fire(enemy.trajectory.next.position, state.me.maxFirePower)
        } else {
          Actions.empty
        }
      }
    } else {
      Actions.empty
    }
  }

  def getFirePerpDeviation(state: WorldState, enemy: Actor): Double = {
    val enemyShipDirection = enemy.speed + enemy.trajectory.next.speed
    val fireDirection = enemy.position - state.me.position
    val fireAngle = enemyShipDirection.angleTo(fireDirection)
    math.abs(90 - fireAngle)
  }
}
