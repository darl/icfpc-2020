package icfpc.classified

import org.scalatest.wordspec.AnyWordSpec

class RendererSpec extends AnyWordSpec {
  "Renderer" should {
    "render to file" in {
      var canvas = Canvas(
        List(
          (1, 3),
          (0, 3),
          (-1, 3),
          (-2, 2),
          (2, 1),
          (1, 1),
          (0, 1),
          (-3, 1),
          (3, 0),
          (1, 0),
          (-1, 0),
          (-3, 0),
          (3, -1),
          (0, -1),
          (-1, -1),
          (-2, -1),
          (2, -2),
          (1, -3),
          (0, -3),
          (-1, -3)
        )
      )
      var canvas2 = Canvas(List((-8, -2), (-7, -3)))
//      (-30 to 30).foreach(i => canvas = canvas.withPoint(i -> i))
//      (-10 to 10).foreach(i => canvas2 = canvas2.withPoint(i -> -i))
      val frame = Renderer.render(canvas, canvas2)

//      while (frame.isActive) { Thread.sleep(1000) }
      succeed
    }
  }
}
