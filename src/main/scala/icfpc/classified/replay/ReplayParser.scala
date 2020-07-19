package icfpc.classified.replay

import icfpc.classified.game.WorldState
import icfpc.classified.syntax.Demodulator

import scala.util.{Failure, Success, Try}

object ReplayParser {

  def parse(lines: Seq[String]): Seq[Try[WorldState]] = {
    lines
      .dropWhile(!_.startsWith("join ="))
      .drop(1)
      .filter(_.startsWith("response:"))
      .map(_.stripPrefix("response: "))
      .map(Demodulator.demodulate)
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
