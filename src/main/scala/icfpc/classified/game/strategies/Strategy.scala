package icfpc.classified.game.strategies

import icfpc.classified.game.Actor.Stats
import icfpc.classified.game.{Actions, WorldState}

trait Strategy {
  def stats(isDefence: Boolean): Stats
  def run(state: WorldState): Actions
}
