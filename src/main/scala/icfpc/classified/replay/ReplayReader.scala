package icfpc.classified.replay

import scala.io.Source

object ReplayReader extends App {
  val replays = Source.fromResource("rep_defender1.txt").getLines().toSeq
  ReplayPlayer(ReplayParser.parseLog(replays)).show()
}
