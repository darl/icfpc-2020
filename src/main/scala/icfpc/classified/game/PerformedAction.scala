package icfpc.classified.game

sealed trait PerformedAction

case class Fired(target: Vector, x1: Int, x2: Int, x3: Int) extends PerformedAction
case class Drove(horizontal: Int, vertical: Int) extends PerformedAction
case object Detonated extends PerformedAction
case object Splitted extends PerformedAction
