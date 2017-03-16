package examples.demo

import examples.demo.ui.{Circle, Clock, Rectangle}
import rescala._

object FGravitationalCircle extends Main {
  override def shapes() = {
    val diameter = Var(50)

    val boundingBoxWidth = Signal{ panelWidth() - 2 * diameter() }
    val boundingBoxHeight = Signal{ panelHeight() - 2 * diameter() }

    val physicsTicks = Clock.time.change.map{ diff => diff.to.get - diff.from.get }

    val accelerationY = 300d / Clock.NanoSecond / Clock.NanoSecond

    val horizontalBounceSources: Var[List[Event[Any]]] = Var(List(Mouse.leftButton.pressed))
    val verticalBounceSources: Var[List[Event[Any]]] = Var(List(Mouse.rightButton.pressed))

    val velocityX = horizontalBounceSources.flatten.fold(150d / Clock.NanoSecond) { (old, _) => -old }
    val velocityY = verticalBounceSources.flatten.zipOuter(physicsTicks).fold(0d) {
      case (old, (Some(_), _)) => -old
      case (old, (None, Some(tick))) => old + tick * accelerationY
    }

    val posX = Mouse.middleButton.pressed.zipOuter(physicsTicks).fold(0d){
      case (_, (Some(Point(x, _)), _)) => x.toDouble
      case (pX, (None, Some(tick))) => pX + tick * velocityX.before
    }
    val posY = Mouse.middleButton.pressed.zipOuter(physicsTicks).fold(0d){
      case (_, (Some(Point(_, y)), _)) => y.toDouble
      case (pY, (None, Some(tick))) => pY + tick * velocityY.before
    }

    val outOfBoundsLeft = Signal{ posX() < -(boundingBoxWidth() - diameter()) / 2 }
    val outOfBoundsRight = Signal{ posX() > (boundingBoxWidth() - diameter()) / 2 }
    val outOfBoundsTop = Signal{ posY() < -(boundingBoxHeight() - diameter()) / 2 }
    val outOfBoundsBottom = Signal{ posY() > (boundingBoxHeight() - diameter()) / 2 }

    val movedOutOfBoundsHorizontal = Signal{ outOfBoundsLeft() || outOfBoundsRight() }.changedTo(true)
    val movedOutOfBoundsVertical = Signal{ outOfBoundsBottom() }.changedTo(true)

    horizontalBounceSources.transform(movedOutOfBoundsHorizontal :: _)
    verticalBounceSources.transform(movedOutOfBoundsVertical :: _)

    (movedOutOfBoundsHorizontal || movedOutOfBoundsVertical).observe { _ => java.awt.Toolkit.getDefaultToolkit().beep() }

    List(
      new Circle(posX.map(_.toInt), posY.map(_.toInt), diameter),
      new Rectangle(Var(0), Var(0), boundingBoxWidth, boundingBoxHeight)
    )
  }
}
