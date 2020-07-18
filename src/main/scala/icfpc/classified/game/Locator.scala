package icfpc.classified.game

import icfpc.classified.Canvas

object Locator {

  def find(shape: Shape, canvas: Canvas): Option[Point] = {
    if (canvas.isEmpty) return None
    (0 to canvas.width - shape.width).foreach { i =>
      (0 to canvas.height - shape.height).foreach { j =>
        val x = i + canvas.minX
        val y = j + canvas.minY
        val contains = shape.points.zipWithIndex.foldLeft(true) {
          case (acc, (row, k)) =>
            acc && row.zipWithIndex.foldLeft(true) {
              case (acc, (pixel, t)) =>
                acc && canvas.points.contains((x + k) -> (y + t)) == pixel
            }
        }
        if (contains) return Some(Point(x, y))
      }
    }
    None
  }
}
