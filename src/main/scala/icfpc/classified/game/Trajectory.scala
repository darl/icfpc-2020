package icfpc.classified.game

import icfpc.classified.game.Trajectory.Position

import scala.collection.mutable.ArrayBuffer

class Trajectory(position: Vector, speed: Vector) {

  private val states = ArrayBuffer.empty[Position]
  states.addOne(Position(position, speed))

  def afterStep(step: Int): Position = {
    while (step > states.size - 1) {
      states.addOne(states(step - 1).advance)
    }
    states(step)
  }

  def next: Position = afterStep(1)

  def next(steps: Int): Iterator[Position] = {
    Iterator.from(1).take(steps).map(afterStep)
  }

  def isFatalIn(steps: Int): Boolean = {
    next(steps).exists(_.isFatal)
  }

}

object Trajectory {

  case class Position(position: Vector, speed: Vector) {

    def advance: Position = {
      val g = Vector(
        if (position.x.abs >= position.y.abs) -position.x.sign else 0,
        if (position.y.abs >= position.x.abs) -position.y.sign else 0
      )
      // workaround antigravity hack
      val newSpeed = if (speed.isZero) Vector.Zero else speed + g
      Position(position + newSpeed, newSpeed)
    }

    def isFatal: Boolean = {
      position.x.abs <= 20 && position.y.abs <= 20
    }
  }
}
