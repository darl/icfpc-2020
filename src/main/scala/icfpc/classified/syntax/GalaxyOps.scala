package icfpc.classified.syntax

import scala.io.Source

object GalaxyOps {

  private val functionDefs: List[FunctionDef] = Source
    .fromResource("galaxy.txt")
    .getLines()
    .takeWhile(_.startsWith(":"))
    .map { line =>
      val parts = line.split(" +").toList
      val (UnknownVariable(id), tail) = Parser.parseText(parts)
      require(tail.head == "=", "expected `=`")
      val (value, rest) = Parser.parseText(tail.tail)
      require(rest.isEmpty, s"expected no more input: $rest")
      FunctionDef(id, value)
    }
    .toList

  val functions: Map[Long, Expression] = functionDefs.map(f => f.id -> f.expr).toMap

  val Galaxy: UnknownVariable = UnknownVariable(1338)

  def main(args: Array[String]): Unit = {
    functionDefs.foreach(println)
  }
}
