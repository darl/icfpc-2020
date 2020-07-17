package icfpc.classified

case class Program(expression: Expression) {

  def execute: Int = {
    expression match {
      case expression: Expression0 =>
        expression.eval match {
          case Variable(result) => result
          case _ => throw new IllegalStateException("Program evaluation is a function")
        }
      case _ => throw new IllegalStateException("Program can't take params yet")
    }
  }
}
