package examples.demo

import examples.demo.ui.{Circle, Shape, ShapesPanel}
import reactives.default.*

import java.awt.Dimension
import scala.swing.{MainFrame, SimpleSwingApplication, UIElement}

/** So far, we showed Vars and Signals defined as a transformation
  * of one other Var or Signal. Signals can, however, also be derived
  * from a combination of multiple other Vars and Signals through
  * Signal expressions. To demonstrate Signal expressions, we extend
  * our animation to scale the moving circle's trajectory with the
  * drawing panel's size.
  *
  * The ShapePanel class offers Signals width and height, that
  * reflect the panel's current size at any point in time, similar
  * to how nsTime always reflects the current system clock. To be
  * able to use these members of ShapesPanel in the definition of
  * posX and posY, we must instantiate the ShapesPanel beforehand,
  * but the instantiation of ShapesPanel, uses posX and posY. But,
  * because ShapesPanel takes a Signal of a List of Shapes in its
  * constructor, we can resolve this circular dependency by
  * instantiating it with a Var containing an initially empty List,
  * and then update the Var to add the Circle at posX and posY at
  * the end. For this update, we use the Var's .transform method.
  *
  * With the ShapesPanel now instantiated at the very beginning,
  * its width and height signal are available when defining posX
  * and posY. We thus now change their definitions into Signal
  * expressions that use a third of the panels width and height
  * as the respective horizontal or vertical radius of the new
  * oval trajectory, with angle still defining the position of
  * the displayed circle on that trajectory.
  */
object DScaledClockCircle extends SimpleSwingApplication {
  val NanoSecond = 1000000000L

  val nsTime = Var(System.nanoTime())
  def tick() = nsTime.set(System.nanoTime())

  val shapes = Var[List[Shape]](List.empty)
  val panel  = new ShapesPanel(shapes)

  val angle = nsTime.map(_.toDouble / NanoSecond * math.Pi)

  val pos = Signal {
    Pos(
      (panel.width.value / 2 - 50).toDouble * math.sin(angle.value),
      (panel.height.value / 2 - 50).toDouble * math.cos(angle.value)
    )
  }

  shapes.transform(new Circle(pos, Var(50)) :: _)

  override lazy val top = {
    panel.preferredSize = new Dimension(400, 300)
    new MainFrame {
      title = "REScala Demo"
      contents = panel
      setLocationRelativeTo(new UIElement { override def peer = null })
    }
  }

  override def main(args: Array[String]): Unit = {
    super.main(args)

    while !top.visible do Thread.sleep(5)
    while top.visible do {
      Thread.sleep(1)
      tick()
    }
  }
}
