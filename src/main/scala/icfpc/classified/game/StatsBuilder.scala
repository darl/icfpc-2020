package icfpc.classified.game

import icfpc.classified.game.Actor.Stats

object StatsBuilder {
  val maxDefence = 448
  val maxAttack = 506

  def build(isDefence: Boolean, stats: Stats): Stats = {
    val points = stats.x * 4 * stats.cooling * 12 + stats.z * 2
    val max = if (isDefence) maxDefence else maxAttack

    Stats(max - points, stats.x, stats.cooling, stats.z)
  }
}
