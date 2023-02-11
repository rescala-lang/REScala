package examples.demo

import examples.demo.GModularClockCircle.Clock
import examples.demo.ui.{Circle, Shape, ShapesPanel}
import rescala.default._

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
      x = (panel.width() / 2 - 50).toDouble * math.sin(angle()) / Clock.NanoSecond,
      y = (panel.height() / 2 - 50).toDouble * math.cos(angle()) / Clock.NanoSecond
    )
  }

  val inc = Clock.ticks.map(tick => velocity.value * tick.toDouble)

  val pos = inc.fold(Pos(0, 0)) { (cur, inc) => cur + inc }

  shapes.transform(new Circle(pos, Var(50)) :: _)
}
