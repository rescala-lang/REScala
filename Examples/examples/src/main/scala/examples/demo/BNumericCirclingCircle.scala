package examples.demo

import examples.demo.ui.{Circle, Clock, Point}
import rescala._

object BNumericCirclingCircle extends Main {
  override def makeShapes() = {
    val physicsTicks = Clock.time.change.map{ diff => diff.to.get - diff.from.get }

    val angle = Signal{ Clock.time().toDouble / Clock.NanoSecond }

    val velocityX = Signal{ panel.width() / 3 * math.sin(angle()) / Clock.NanoSecond }
    val velocityY = Signal{ panel.height() / 3 * math.cos(angle()) / Clock.NanoSecond }

    val posX = panel.Mouse.middleButton.pressed.zipOuter(physicsTicks).fold(0d){
      case (_, (Some(Point(x, _)), _)) => x.toDouble
      case (pX, (None, Some(tick))) => pX + tick * velocityX.before
    }
    val posY = panel.Mouse.middleButton.pressed.zipOuter(physicsTicks).fold(0d){
      case (_, (Some(Point(_, y)), _)) => y.toDouble
      case (pY, (None, Some(tick))) => pY + tick * velocityY.before
    }

    List(new Circle(posX.map(_.toInt), posY.map(_.toInt), Var(50)))
  }
}
