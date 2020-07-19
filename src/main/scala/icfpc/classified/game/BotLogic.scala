package icfpc.classified.game

class BotLogic {

  def run(state: WorldState): Actions = {
    Actions.drive(state.me.speed * -1) |+|
      Actions.fire(state.enemy.position + state.enemy.speed)
  }
}
