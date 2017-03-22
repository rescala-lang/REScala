package examples.demo

import java.awt.Dimension

import examples.demo.ui.{Circle, Shape, ShapesPanel}
import rescala._

import scala.swing.{MainFrame, SimpleSwingApplication, UIElement}

/**
  * As with the initial arithmetic trajectory, the numeric integration so far
  * is vulnerable to clock drift. To fix this, we derive integration ticks from
  * the accurately system clock-based nsTime Signal, using a conversion in the
  * other direction, from Signals to Events. Signal provide a change method,
  * which derives an event stream that emits a tuple of values (old, new)
  * whenever the signal is updated. From this, we derive another event stream
  * that transforms each of these tuples into a number by subtracting the old
  * value from the new, thereby emitting the time elapsed on every nsTime
  * update. Accordingly, the manual triggering of ticks in the main thread
  * is no longer necessary.
  */
object FClockNumericCircle extends SimpleSwingApplication {
  val NanoSecond = 1e9d

  val nsTime = Var(System.nanoTime())
  def tick() = nsTime.set(System.nanoTime())

  val ticks = nsTime.change.map{ diff => diff.to.get - diff.from.get }

  val shapes = Var[List[Shape]](List.empty)
  val panel = new ShapesPanel(shapes)

  val angle = nsTime.map( _ / NanoSecond * math.Pi)

  val velocityX = Signal{ panel.width() / 3 * math.sin(angle()) / NanoSecond}
  val velocityY = Signal{ panel.height() / 3 * math.cos(angle()) / NanoSecond }

  val posX = ticks.fold(0d){ (pX, tick) => pX + tick * velocityX.before }
  val posY = ticks.fold(0d){ (pY, tick) => pY + tick * velocityY.before }

  shapes.transform(new Circle(posX.map(_.toInt), posY.map(_.toInt), Var(50)) :: _)

  override val top = {
    panel.preferredSize = new Dimension(400, 300)
    new MainFrame {
      title = "REScala Demo"
      contents = panel
      setLocationRelativeTo(new UIElement { override def peer = null })
    }
  }

  override def main(args: Array[String]): Unit = {
    super.main(args)

    while(!top.visible) Thread.sleep(5)
    while(top.visible) {
      Thread.sleep(1)
      tick()
    }
  }
}
