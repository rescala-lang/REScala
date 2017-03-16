package examples.demo

import examples.demo.ui.{Circle, Clock}
import rescala._

object AArithmeticCirclingCircle extends Main {
  override def shapes() = {
    val diameter = Var(50)

    val angle = Signal{ Clock.time().toDouble / Clock.NanoSecond }

    val posX = Signal{ panelWidth() / 3 * math.sin(angle()) }
    val posY = Signal{ panelHeight() / 3 * math.cos(angle()) }

    List(
      new Circle(posX.map(_.toInt), posY.map(_.toInt), diameter)
    )
  }
}
