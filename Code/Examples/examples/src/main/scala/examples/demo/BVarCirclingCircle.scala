package examples.demo

import java.awt.Dimension

import examples.demo.ui.{Circle, ShapesPanel}
import rescala.default._

import scala.swing.{MainFrame, SimpleSwingApplication, UIElement}

/** We define a new Var angle. We modify the main method
  * to repeatedly assign a new value to angle. Angle changes
  * by about 1 Pi per second, although this is obviously
  * inaccurate.
  *
  * We use derive from angle the x and y position of a point
  * on a 100 pixels wide circle. We remove all shapes from
  * our display except a single circle, whose position we
  * replace by the posX and posY values.
  *
  * When executing this application, it now shows an animation
  * instead of a static image, despite the main thread updating
  * only angle, neither posX nor posY, nor the circle, nor
  * the UI. This is because Vars and Signals automatically
  * propagate changes: When the main thread assigns a new
  * value to angle, posX and posY are automatically
  * recomputed, because they depend on angle. Equally, the
  * UI display depends on the properties of all displayed
  * shapes, i.e., posX and posY here. Thus, after posX and
  * posY were updated, the UI is automatically redrawn, too.
  */
object BVarCirclingCircle extends SimpleSwingApplication {
  val angle = Var(0d)

  val pos = angle.map(a => Pos(100d * math.sin(a), 100d * math.cos(a)))

  override lazy val top = {
    val panel = new ShapesPanel(Var(List(
      new Circle(pos, Var(50))
    )))
    panel.preferredSize = new Dimension(400, 300)
    new MainFrame {
      title = "REScala Demo"
      contents = panel
      setLocationRelativeTo(new UIElement { override def peer = null })
    }
  }

  override def main(args: Array[String]): Unit = {
    super.main(args)

    while (!top.visible) Thread.sleep(5)
    while (top.visible) {
      Thread.sleep(1)
      angle.transform(_ + math.Pi / 1000d)
    }
  }
}
