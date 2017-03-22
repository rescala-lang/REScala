package examples.demo

import examples.demo.system.Clock
import examples.demo.ui.Circle
import rescala._

object AArithmeticCirclingCircle extends Main {
  override def makeShapes() = {
    val angle = Signal{ Clock.nsTime().toDouble / Clock.NanoSecond }

    val posX = Signal{ panel.width() / 3 * math.sin(angle()) }
    val posY = Signal{ panel.height() / 3 * math.cos(angle()) }

    List(new Circle(posX.map(_.toInt), posY.map(_.toInt), Var(50)))
  }
}
