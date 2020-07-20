package icfpc.classified.game.strategies
import icfpc.classified.game.Actor.Stats
import icfpc.classified.game.{Actions, Actor, WorldState, Vector}

object Defender extends Strategy {
  private val minionsNumber = 60
  private val startSplitMove = 35
  override def stats(isDefence: Boolean): Actor.Stats = Stats(0, 0, 8, minionsNumber + 1)

  var flyingAway = true

  override def run(state: WorldState): Actions = {
    val targetForce = if (flyingAway && math.abs(state.me.position.x) < 120 && math.abs(state.me.position.y) < 120) {
      val targetSpeed = (state.me.position.defaultNormal * 1.2 + state.me.position.normalize).widthLength(7)
      targetSpeed - state.me.speed
    } else {
      flyingAway = false
      val targetSpeed = state.me.position.defaultNormal.widthLength(7)
      targetSpeed - state.me.speed
    }

    val move =
      if (state.me.canDrive && (targetForce.length > 3 || state.moveNumber < 15) || state.me.trajectory.isFatalIn(16)) {
        Actions.moveDirection(targetForce)
      } else Actions.empty

    val split =
      if (canSplit(state)) Actions.split(Stats(0, 0, 0, 1))
      else Actions.empty

    move |+| split
  }

  private def canSplit(state: WorldState): Boolean = {
    state.me.stats.z > 1 && state.moveNumber > startSplitMove && !state.me.trajectory.isFatalIn(10)
  }
}
