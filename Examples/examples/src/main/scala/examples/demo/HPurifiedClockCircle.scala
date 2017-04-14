package examples.demo

import examples.demo.GModularClockCircle.Clock
import examples.demo.ui.{Circle, Shape, ShapesPanel}
import rescala._

/**
  * As Clock is now a closed module, we can move it to an external
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
  val panel = new ShapesPanel(shapes)

  val angle = Clock.nsTime.map( _.toDouble / Clock.NanoSecond * math.Pi)

  val velocityX = Signal {(panel.width() / 2 - 50).toDouble * math.sin(angle()) / Clock.NanoSecond}
  val velocityY = Signal {(panel.height() / 2 - 50).toDouble * math.cos(angle()) / Clock.NanoSecond}

  val posX = Clock.ticks.fold(0d){ (pX, tick) => pX + tick.toDouble * velocityX.before }
  val posY = Clock.ticks.fold(0d){ (pY, tick) => pY + tick.toDouble * velocityY.before }

  shapes.transform(new Circle(posX.map(_.toInt), posY.map(_.toInt), Var(50)) :: _)
}
