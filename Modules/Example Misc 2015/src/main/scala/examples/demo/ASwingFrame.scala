package examples.demo

import java.awt.Dimension

import examples.demo.ui.{Circle, Rectangle, ShapesPanel}
import rescala.default._

import examples.demo.ui.Shape

import scala.swing.{MainFrame, SimpleSwingApplication, UIElement}

/** This is a static display of two circles and a rectangle.
  * It demonstrates, how to display Shapes using our custom
  * ShapesPanel. The only REScala Feature used here are Vars,
  * which we explain in the next step.
  */
object ASwingFrame extends SimpleSwingApplication {
  override lazy val top = {
    val panel = new ShapesPanel(Var(List[Shape](
      new Circle(center = Var(Pos(75, 30)), diameter = Var(25)),
      new Circle(Var(Pos(100, 100)), Var(50)),
      new Rectangle(centerX = Var(-50), centerY = Var(-100), hitboxWidth = Var(10), hitboxHeight = Var(100))
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
      /* TODO main loop */
    }
  }
}
