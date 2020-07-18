package icfpc.classified

import org.scalatest.wordspec.AnyWordSpec

class RendererSpec extends AnyWordSpec {
  "Renderer" should {
    "render to file" in {
      var canvas = Canvas(List.empty)
      (1 to 300).foreach(i => canvas = canvas.withPoint(i -> i))
      val frame = Renderer.render(canvas)

      while (frame.isActive) { Thread.sleep(1000) }
      succeed
    }
  }
}
