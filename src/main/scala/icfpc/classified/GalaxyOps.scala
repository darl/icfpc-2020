package icfpc.classified

import scala.io.Source

object GalaxyOps {

  val functions: List[FunctionDef] = Source
    .fromResource("galaxy.txt")
    .getLines()
    .takeWhile(_.startsWith(":"))
    .map { line =>
      val parts = line.split(" +").toList
      val (UnknownVariable(id), tail) = Parser.parseText(parts)
      require(tail.head == "=", "expected `=`")
      val (value, rest) = Parser.parseText(tail.tail)
      require(rest.isEmpty, s"expected no mo input: $rest")
      FunctionDef(id, value)
    }
    .toList

  val Galaxy: UnknownVariable = UnknownVariable(1338)

  def main(args: Array[String]): Unit = {
    functions.foreach(println)
  }
}
