package icfpc.classified.replay

import icfpc.classified.sandbox.StateAnnotator

import scala.io.Source

object ReplayReader extends App {
  val replays = Source.fromResource("rep_defender1.txt").getLines().toSeq
  val (worlds, states) = ReplayParser.parseLog(replays)
  ReplayPlayer(worlds, states = states.map(s => StateAnnotator.annotate(s))).show()
}
