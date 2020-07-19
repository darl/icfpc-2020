package icfpc.classified.game

import icfpc.classified.game.WorldState.Started

object ReplayReader {

  def read(name: String): Seq[WorldState] = {
    val a1 = Actor(Vector(-10, 30), Vector(0, 0), 100)
    val a2 = Actor(Vector(-12, 28), Vector(0, 0), 100)
    val d1 = Actor(Vector(30, -7), Vector(0, 0), 100)
    val d2 = Actor(Vector(35, -14), Vector(0, 0), 100)
    Seq(
      WorldState(Started, a1, d1, true),
      WorldState(Started, a2, d2, true)
    )
  }
}
