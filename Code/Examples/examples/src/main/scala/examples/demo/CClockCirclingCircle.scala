package examples.demo

import java.awt.Dimension

import examples.demo.ui.{Circle, ShapesPanel}
import rescala.default._

import scala.swing.{MainFrame, SimpleSwingApplication, UIElement}

/** In an effort to make the animation's timing more accurate,
  * we refactor the application to use the actual System's time.
  * We introduce a Var nsTime, which we assign the System's
  * current timestamp. We add a tick() method, which re-assigns
  * the updated timestamp when called. We place the according
  * call in the main method, meaning nsTime will be kept
  * up-to-date throughout the application's execution.
  * Given nsTime, we redefine angle as a derivation of nsTime,
  * thus now accurately rotating by 1 Pi per second without
  * time drift.
  */
object CClockCirclingCircle extends SimpleSwingApplication {
  val NanoSecond = 1000000000L

  val nsTime = Var(System.nanoTime())
  def tick() = nsTime.set(System.nanoTime())

  val angle = nsTime.map(_.toDouble / NanoSecond * math.Pi)

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
      tick()
    }
  }
}
