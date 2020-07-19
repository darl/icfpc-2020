package icfpc.classified.replay

import icfpc.classified.game.WorldState
import icfpc.classified.syntax.{Demodulator, Expression}

import scala.util.{Failure, Success, Try}

object ReplayParser {

  def parseLog(lines: Seq[String]): (Seq[Try[WorldState]], Seq[Expression]) = {
    val states = lines
      .dropWhile(!_.startsWith("join ="))
      .drop(1)
      .filter(_.startsWith("response:"))
      .map(_.stripPrefix("response: "))
      .map(Demodulator.demodulate)
    (render(states), states)
  }

  def render(states: Seq[Expression]): Seq[Try[WorldState]] = {
    states
      .map(state =>
        Try(WorldState.parse(state)).transform(
          Success.apply,
          err => {
            Failure(new IllegalStateException(s"Error parsing state [$state] to WorldSpec", err))
          }
        )
      )
  }
}
