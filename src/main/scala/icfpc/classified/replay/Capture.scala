package icfpc.classified.replay

import scala.collection.mutable

sealed trait Capture[T] {
  def log(elem: T): Unit
  def elems: Seq[T]
}

class MutableCapture[T] extends Capture[T] {
  val acc: mutable.Buffer[T] = mutable.Buffer[T]()

  override def log(elem: T): Unit = acc.append(elem)

  override def elems: Seq[T] = acc.toSeq
}

class NoOpCapture[T] extends Capture[T] {
  override def log(elem: T): Unit = ()

  override def elems: Seq[T] = Seq.empty
}

object Capture {
  implicit def default[T]: Capture[T] = noOp[T]

  def mutable[T]: Capture[T] = new MutableCapture[T]
  def noOp[T]: Capture[T] = new NoOpCapture[T]
}
