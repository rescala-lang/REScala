package examples.demo

import examples.demo.FModularClockCircle.Clock
import examples.demo.ui.{Circle, Point, Shape, ShapesPanel}
import rescala._

object HNumericResettableCircle extends Main {
  val shapes = Var[List[Shape]](List.empty)
  val panel = new ShapesPanel(shapes)

  val angle = Clock.nsTime.map( _ / NanoSecond * math.Pi)

  val velocityX = Signal{ panel.width() / 3 * math.sin(angle()) / Clock.NanoSecond}
  val velocityY = Signal{ panel.height() / 3 * math.cos(angle()) / Clock.NanoSecond }

  val posX = panel.Mouse.middleButton.pressed.zipOuter(physicsTicks).fold(0d){
    case (_, (Some(Point(x, _)), _)) => x.toDouble
    case (pX, (None, Some(tick))) => pX + tick * velocityX.before
  }
  val posY = panel.Mouse.middleButton.pressed.zipOuter(physicsTicks).fold(0d){
    case (_, (Some(Point(_, y)), _)) => y.toDouble
    case (pY, (None, Some(tick))) => pY + tick * velocityY.before
  }

  shapes.transform(new Circle(posX.map(_.toInt), posY.map(_.toInt), Var(50)) :: _)
}
