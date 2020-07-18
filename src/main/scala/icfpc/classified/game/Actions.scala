package icfpc.classified.game

import Actions._
import icfpc.classified._

case class Actions(
    drive: Option[Drive],
    fire: Option[Fire],
    sit: Option[Boolean]) {

  def |+|(other: Actions): Actions = {
    Actions(other.drive.orElse(drive), other.fire.orElse(fire), other.sit.orElse(sit))
  }

  def serialize: Expression = {
    var commands: List[Expression] = List.empty
    drive.foreach { drive =>
      commands = drive.serialize :: commands
    }
    sit.foreach { sit =>
      if (sit) {
        commands = makeList(1, 0) :: commands
      }
    }
    fire.foreach { fire =>
      commands = fire.serialize :: commands
    }

    makeList(commands: _*)
  }
}

object Actions {
  def empty: Actions = Actions(None, None, None)

  def fire(coordinates: Vector): Actions = Actions(None, Some(Fire(coordinates)), None)

  case class Drive(horizontal: Int, vertical: Int) {

    def +(other: Drive): Drive =
      Drive(
        horizontal = horizontal + other.horizontal,
        vertical = vertical + other.vertical
      )

    def serialize: Expression = makeList(0, 0, pair(horizontal, vertical))
  }

  object Drive {
    val up: Drive = Drive(0, -1)
    val down: Drive = Drive(0, 1)
    val right: Drive = Drive(1, 0)
    val left: Drive = Drive(-1, 0)
  }

  case class Fire(coordinates: Vector) {

    def serialize: Expression = {
      makeList(2, 0, pair(coordinates.x, coordinates.y), 86)
    }
  }

}
