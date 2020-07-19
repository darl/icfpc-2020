package icfpc.classified.game

import Actions._
import icfpc.classified.game.Actor.Stats
import icfpc.classified.syntax._
import icfpc.classified.syntax.Expression

case class Actions(
    drive: Option[Drive] = None,
    fire: Option[Fire] = None,
    detonate: Option[Detonate] = None,
    split: Option[Split] = None) {

  def |+|(other: Actions): Actions = {
    Actions(other.drive.orElse(drive), other.fire.orElse(fire), other.detonate.orElse(detonate))
  }

  override def toString: String =
    productIterator
      .map {
        case None => ""
        case Some(a) => a.toString
        case a => a.toString
      }
      .filter(_.nonEmpty)
      .mkString("[", ", ", "]")

  def serialize(state: WorldState): Expression = {
    var commands: List[Expression] = List.empty
    drive.foreach { drive =>
      commands = drive.serialize(state) :: commands
    }
    detonate.foreach { detonate =>
      commands = detonate.serialize(state) :: commands
    }
    fire.foreach { fire =>
      commands = fire.serialize(state) :: commands
    }
    split.foreach { split =>
      commands = split.serialize(state) :: commands
    }

    makeList(commands: _*)
  }
}

object Actions {
  def empty: Actions = Actions(None, None, None)

  def fire(coordinates: Vector): Actions = Actions(None, Some(Fire(coordinates)), None)

  def drive(direction: Vector): Actions = {
    val normX = direction.x.max(-1).min(1).toInt
    val normY = direction.y.max(-1).min(1).toInt
    Actions(drive = Some(Drive(normX, normY)))
  }

  def moveDirection(direction: Vector): Actions = {
    drive(direction * -1)
  }

  case class Drive(horizontal: Int, vertical: Int) {

    def +(other: Drive): Drive =
      Drive(
        horizontal = horizontal + other.horizontal,
        vertical = vertical + other.vertical
      )

    def serialize(state: WorldState): Expression =
      makeList(0, if (state.isDefence) 0 else 1, pair(horizontal, vertical))
  }

  object Drive {
    val up: Drive = Drive(0, -1)
    val down: Drive = Drive(0, 1)
    val right: Drive = Drive(1, 0)
    val left: Drive = Drive(-1, 0)
  }

  case class Fire(coordinates: Vector) {

    def serialize(state: WorldState): Expression = {
      makeList(2, if (state.isDefence) 0 else 1, pair(coordinates.x.toInt, coordinates.y.toInt), 86)
    }
  }

  case class Detonate() {

    def serialize(state: WorldState): Expression = {
      makeList(1, if (state.isDefence) 0 else 1)
    }
  }

  case class Split(stats: Stats) {

    def serialize(state: WorldState): Expression = {
      makeList(3, if (state.isDefence) 0 else 1, stats.asList)
    }
  }
}
