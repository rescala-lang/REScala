package examples.demo

import examples.demo.system.Clock
import examples.demo.ui.{Circle, Point}
import rescala._

object BNumericCirclingCircle extends Main {
  override def makeShapes() = {
    val physicsTicks = Clock.nsTime.change.map{ diff => diff.to.get - diff.from.get }

    val angle = Signal{ Clock.nsTime().toDouble / Clock.NanoSecond }

    val velocityX = Signal{ panel.width() / 3 * math.sin(angle()) / Clock.NanoSecond }
    val velocityY = Signal{ panel.height() / 3 * math.cos(angle()) / Clock.NanoSecond }

    val posX = physicsTicks.fold(0d){ (pX, tick) => pX + tick * velocityX.before }
    val posY = physicsTicks.fold(0d){ (pY, tick) => pY + tick * velocityY.before }

    List(new Circle(posX.map(_.toInt), posY.map(_.toInt), Var(50)))
  }
}
