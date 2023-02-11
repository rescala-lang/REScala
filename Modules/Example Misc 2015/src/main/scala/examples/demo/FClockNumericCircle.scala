package examples.demo

import java.awt.Dimension

import examples.demo.ui.{Circle, Shape, ShapesPanel}
import rescala.default._
import rescala.operator.Diff

import scala.swing.{MainFrame, SimpleSwingApplication, UIElement}

/** As with the initial arithmetic trajectory, the numeric integration so far
  * is vulnerable to clock drift. To fix this, we derive integration ticks from
  * the accurately system clock-based nsTime Signal, using a conversion in the
  * other direction, from Signals to Events. Signal provide a change method,
  * which derives an event stream that emits a tuple of values (old, new)
  * whenever the signal is updated. From this, we derive another event stream
  * that transforms each of these tuples into a number by subtracting the old
  * value from the new. This essentially implements numeric differentiation,
  * emitting the time elapsed between each nsTime update. Accordingly, the
  * manual triggering of ticks in the main thread is no longer necessary.
  */
object FClockNumericCircle extends SimpleSwingApplication {
  val NanoSecond = 1000000000L

  val nsTime = Var(System.nanoTime())
  def tick() = nsTime.set(System.nanoTime())

  val ticks = nsTime.change.collect { case Diff(from, to) => to - from }

  val shapes = Var[List[Shape]](List.empty)
  val panel  = new ShapesPanel(shapes)

  val angle = nsTime.map(_.toDouble / NanoSecond * math.Pi)

  val velocity = Signal {
    Pos(
      x = (panel.width() / 2 - 50).toDouble * math.sin(angle()) / NanoSecond,
      y = (panel.height() / 2 - 50).toDouble * math.cos(angle()) / NanoSecond
    )
  }

  val inc = ticks.map(tick => velocity.value * tick.toDouble)

  val pos = inc.fold(Pos(0, 0)) { (cur, inc) => cur + inc }

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

    while (!top.visible) Thread.sleep(5)
    while (top.visible) {
      Thread.sleep(1)
      tick()
    }
  }
}
