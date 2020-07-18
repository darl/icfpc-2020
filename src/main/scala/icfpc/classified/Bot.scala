package icfpc.classified

case class Bot(action: (Int, Int) => Seq[Canvas]) {

  def joinGame: Unit = {
    action(26, 0)
  }
}
