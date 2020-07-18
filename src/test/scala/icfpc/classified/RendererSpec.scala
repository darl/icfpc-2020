package icfpc.classified

import org.scalatest.wordspec.AnyWordSpec

class RendererSpec extends AnyWordSpec {
  "Renderer" should {
    "render to file" in {
      var canvas = Canvas(List.empty)
      var canvas2 = Canvas(List.empty)
      (-30 to 30).foreach(i => canvas = canvas.withPoint(i -> i))
      (-10 to 10).foreach(i => canvas2 = canvas2.withPoint(i -> -i))
      val frame = Renderer.render(canvas, canvas2)

      while (frame.isActive) { Thread.sleep(1000) }
      succeed
    }
  }
}
