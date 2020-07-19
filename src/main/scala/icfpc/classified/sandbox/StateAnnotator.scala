package icfpc.classified.sandbox

import icfpc.classified.syntax._

object StateAnnotator {

  private val knownData: Map[String, String] = Map(
//    "0." -> "gameStage",
//    "2.1." -> "isDefender",
//    "3.2.0.0.2." -> "defenderPosition",
//    "3.2.0.1." -> "defenderCommands",
//    "3.2.1.0.2." -> "attackerPosition",
//    "3.2.1.1." -> "attackerCommands"
  )

  def annotate(state: Expression, filter: String = ""): String = {
    def inner(expression: Expression, locator: String): Any = {
      knownData.getOrElse(
        locator,
        expression match {
          case Cons(Literal(left), Literal(right)) => left -> right
          case Nil => List.empty
          case cons: Cons =>
            val res = cons.toList.zipWithIndex
              .map {
                case (elem, i) =>
                  elem -> s"$locator$i."
              }
              .collect {
                case (elem, innerLocator) if innerLocator.startsWith(filter.take(innerLocator.length)) =>
                  inner(elem, innerLocator)
              }
            if (res.size == 1) res.head else res
          case l: Literal => l.value.toInt
          case other => throw new IllegalStateException(s"$other")
        }
      )

    }
    pprint.apply(inner(state, ""), 60).plainText
  }
}
