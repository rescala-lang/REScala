package examples.demo

import java.awt.Color

import examples.demo.ui.{Circle, Clock, Rectangle}
import rescala._

object CBoundsCirclingCircle extends Main {
  override def shapes() = {
    val diameter = Var(50)

    val boundingBoxWidth = Signal{ panelWidth() - 2 * diameter() }
    val boundingBoxHeight = Signal{ panelHeight() - 2 * diameter() }

    val physicsTicks = Clock.time.change.map{ diff => diff.to.get - diff.from.get }

    val angle = Signal{ Clock.time().toDouble / Clock.NanoSecond }

    val velocityX = Signal{ panelWidth() / 3 * math.sin(angle()) / Clock.NanoSecond }
    val velocityY = Signal{ panelHeight() / 3 * math.cos(angle()) / Clock.NanoSecond }

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

    List(
      new Circle(posX.map(_.toInt), posY.map(_.toInt), diameter),
      new Rectangle(Var(0), Var(0), boundingBoxWidth, boundingBoxHeight, Signal{
        if(outOfBoundsLeft() || outOfBoundsRight() || outOfBoundsTop() || outOfBoundsBottom())
          Some(Color.RED) else Some(Color.GREEN)
      })
    )
  }
}
