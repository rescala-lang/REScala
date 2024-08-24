package ex201x.programming2016demo

import ex201x.programming2016demo.GModularClockCircle.Clock
import ex201x.programming2016demo.ui.{Circle, Shape, ShapesPanel}
import reactives.default.*

/** As Clock is now a closed module, we can move it to an external
  * file and simply use it externally. Unlike purely object-oriented
  * designs, RP abstractions remain reactive across such module
  * separations without implementing callback-based listener
  * interfaces.
  *
  * Further, to reduce clutter, we move the UI instantiation and
  * main loop into an external superclass.
  */
object HPurifiedClockCircle extends Main {
  val shapes = Var[List[Shape]](List.empty)
  val panel  = new ShapesPanel(shapes)

  val angle = Clock.nsTime.map(_.toDouble / Clock.NanoSecond * math.Pi)

  val velocity = Signal {
    Pos(
      x = (panel.width.value / 2 - 50).toDouble * math.sin(angle.value) / Clock.NanoSecond,
      y = (panel.height.value / 2 - 50).toDouble * math.cos(angle.value) / Clock.NanoSecond
    )
  }

  val inc = Clock.ticks.map(tick => velocity.value * tick.toDouble)

  val pos = inc.fold(Pos(0, 0)) { (cur, inc) => cur + inc }

  shapes.transform(new Circle(pos, Var(50)) :: _)
}
