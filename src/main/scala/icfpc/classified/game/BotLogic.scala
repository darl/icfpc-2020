package icfpc.classified.game

class BotLogic {

  def run(state: WorldState): Actions = {
    val targetSpeed = state.me.position.normal.widthLength(7)
    val targetForce = targetSpeed - state.me.speed
    val move = moveSafely(targetForce, state.me.heat)

    val fire = fireDefault(state)

    move |+| fire
  }

  def moveSafely(targetForce: Vector, curHeat: Int): Actions =
    if (curHeat < 48 && targetForce.length > 2) Actions.moveDirection(targetForce)
    else Actions.empty

  def fireDefault(state: WorldState): Actions =
    if (state.me.heat <= 32) {
      val g = state.enemy.position.normalize.!
      val fireDirection = state.enemy.position + state.enemy.speed + g
      Actions.fire(fireDirection.round)
    } else Actions.empty
}
