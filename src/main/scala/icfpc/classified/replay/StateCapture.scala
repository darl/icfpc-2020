package icfpc.classified.replay

import icfpc.classified.syntax.Expression

import scala.collection.mutable

sealed trait StateCapture {
  def log(state: Expression): Unit
  def states: Seq[Expression]
}

class MutableStateCapture extends StateCapture {
  val acc: mutable.Buffer[Expression] = mutable.Buffer[Expression]()

  override def log(state: Expression): Unit = acc.append(state)

  override def states: Seq[Expression] = acc.toSeq
}

object NoOpCapture extends StateCapture {
  override def log(state: Expression): Unit = ()

  override def states: Seq[Expression] = Seq.empty
}

object StateCapture {
  implicit val default: StateCapture = NoOpCapture

  def mutable: StateCapture = new MutableStateCapture
  def noOp: StateCapture = NoOpCapture
}
