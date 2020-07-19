package icfpc.classified.game

import scala.collection.mutable.ArrayBuffer

class Trajectory(position: Vector, speed: Vector) {

  case class State(position: Vector, speed: Vector) {

    def advance: State = {
      val g = position.normalize.!
      val newSpeed = speed + g
      State(position + newSpeed, newSpeed)
    }
  }

  private val states = ArrayBuffer.empty[State]
  states.addOne(State(position, speed))

  def afterStep(step: Int): State = {
    while (step > states.size) {
      states(step) = states(step - 1).advance
    }
    states(step)
  }

  def next: State = afterStep(1)

}
