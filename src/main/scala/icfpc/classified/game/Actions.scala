package icfpc.classified.game

import Actions._
import icfpc.classified.game.Actor.Stats
import icfpc.classified.syntax._
import icfpc.classified.syntax.Expression

case class Actions(
    drive: List[Drive] = List.empty,
    fire: List[Fire] = List.empty,
    detonate: List[Detonate] = List.empty,
    split: List[Split] = List.empty) {

  def |+|(other: Actions): Actions = {
    Actions(drive ::: other.drive, fire ::: other.fire, detonate ::: other.detonate, split ::: other.split)
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
      commands = detonate.serialize(state.me) :: commands
    }
    fire.foreach { fire =>
      commands = fire.serialize(state.me) :: commands
    }
    split.foreach { split =>
      commands = split.serialize(state.me) :: commands
    }

    makeList(commands: _*)
  }
}

object Actions {
  def empty: Actions = Actions()

  def fire(coordinates: Vector, power: Int): Actions = Actions(fire = List(Fire(coordinates, power)))

  def drive(direction: Vector): Actions = {
    val normX = direction.x.max(-1).min(1).toInt
    val normY = direction.y.max(-1).min(1).toInt
    Actions(drive = List(Drive(normX, normY)))
  }

  val detonate: Actions = Actions(detonate = List(Detonate()))

  def split(stats: Stats): Actions = Actions(split = List(Split(stats)))

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

  case class Fire(coordinates: Vector, power: Int) {

    def serialize(actor: Actor): Expression = {
      makeList(2, actor.shipId, pair(coordinates.x.toInt, coordinates.y.toInt), power)
    }
  }

  case class Detonate() {

    def serialize(actor: Actor): Expression = {
      makeList(1, actor.shipId)
    }
  }

  case class Split(stats: Stats) {

    def serialize(actor: Actor): Expression = {
      makeList(3, actor.shipId, stats.asList)
    }
  }
}
