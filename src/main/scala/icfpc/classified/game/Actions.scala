package icfpc.classified.game

import Actions._

case class Actions(
    drive: Option[Drive],
    fire: Option[Fire])

object Actions {
  def empty: Actions = Actions(None, None)

  def fire(coordinates: Vector): Actions = Actions(None, Some(Fire(coordinates)))

  case class Drive(direction: Int)

  case class Fire(coordinates: Vector)

}
