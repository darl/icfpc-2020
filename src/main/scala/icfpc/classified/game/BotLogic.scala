package icfpc.classified.game

class BotLogic {

  def run(state: WorldState): Actions = {
    Actions.fire(state.enemy.position)
  }
}
