package icfpc.classified.game

class BotLogic {

  def run(state: WorldState): Actions = {
    val targetSpeed = state.me.position.normal.widthLength(7)
    val targetForce = targetSpeed - state.me.speed
    val move = Actions.moveDirection(targetForce)

    val fire =
      if (state.me.heat > 48) Actions.empty
      else Actions.fire(state.enemy.position + state.enemy.speed)

    move |+| fire
  }

}
