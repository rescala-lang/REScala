package examples.demo.gameobjects

import examples.demo.system.Clock
import examples.demo.ui.{Circle, Point}
import rescala._

class BouncingCircle(val diameter: Signal[Int], val reset: Event[Point]) {
  val physicsTicks = Clock.nsTime.change.map{ diff => diff.to.get - diff.from.get }

  val angle = Signal{ Clock.nsTime().toDouble / Clock.NanoSecond }

  val horizontalBounceSources: Var[List[Event[Any]]] = Var(List())
  val verticalBounceSources: Var[List[Event[Any]]] = Var(List())

  val velocityX = horizontalBounceSources.flatten.fold(150d / Clock.NanoSecond) { (old, _) => -old }
  val velocityY = verticalBounceSources.flatten.fold(100d / Clock.NanoSecond) { (old, _ ) => -old }

  val posX = reset.zipOuter(physicsTicks).fold(0d){
    case (_, (Some(Point(x, _)), _)) => x.toDouble
    case (pX, (None, Some(tick))) => pX + tick * velocityX.before
  }
  val posY = reset.zipOuter(physicsTicks).fold(0d){
    case (_, (Some(Point(_, y)), _)) => y.toDouble
    case (pY, (None, Some(tick))) => pY + tick * velocityY.before
  }

  val shape = new Circle(posX.map(_.toInt), posY.map(_.toInt), diameter)
}
