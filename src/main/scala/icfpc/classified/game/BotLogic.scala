package icfpc.classified.game

class BotLogic {

  def run(state: WorldState): Actions = {
    val targetSpeed = state.me.position.normal.widthLength(7)
    val targetForce = targetSpeed - state.me.speed
    Actions.moveDirection(targetForce) |+|
      Actions.fire(state.enemy.position + state.enemy.speed)
  }

}
