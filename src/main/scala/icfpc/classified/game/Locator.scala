package icfpc.classified.game

import icfpc.classified.Canvas

object Locator {

  def find(shape: Shape, canvas: Canvas): Option[Point] = {
    if (canvas.isEmpty) return None
    (0 to canvas.width - shape.width).foreach { i =>
      (0 to canvas.height - shape.height).foreach { j =>
        val 
        val contains = shape.points.foldLeft(true) {
          case (acc, row) =>
            acc && row.foldLeft(true) {
              case (acc, pixel) =>
                acc && canvas.points.contains(i -> j) == pixel
            }
        }
        if (contains) return Some(Point(i, j))
      }
    }
    None
  }
}
