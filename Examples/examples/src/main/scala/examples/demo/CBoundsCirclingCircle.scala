package examples.demo

import java.awt.Color

import examples.demo.ui._
import rescala._

object CBoundsCirclingCircle extends Main {
  class BouncingCircle(val diameter: Signal[Int], val reset: Event[Point]) {
    val physicsTicks = Clock.time.change.map{ diff => diff.to.get - diff.from.get }

    val angle = Signal{ Clock.time().toDouble / Clock.NanoSecond }

    val velocityX = Signal{ panel.width() / 3 * math.sin(angle()) / Clock.NanoSecond }
    val velocityY = Signal{ panel.height() / 3 * math.cos(angle()) / Clock.NanoSecond }

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

  class BoundingBox(val width: Signal[Int], val height: Signal[Int], val boundShape: Shape) {
    val centerBBHorizontalDistance = Signal{ (width() - boundShape.hitboxWidth()) / 2 }
    val centerBBVerticalDistance = Signal{ (height() - boundShape.hitboxHeight()) / 2 }

    val outOfBoundsLeft = Signal{ boundShape.centerX() < -centerBBHorizontalDistance() }
    val outOfBoundsRight = Signal{ boundShape.centerX() > centerBBHorizontalDistance() }
    val outOfBoundsTop = Signal{ boundShape.centerY() < -centerBBVerticalDistance() }
    val outOfBoundsBottom = Signal{ boundShape.centerY() > centerBBVerticalDistance() }

    val shape = new Rectangle(Var(0), Var(0), width, height, Signal{
      if(outOfBoundsLeft() || outOfBoundsRight() || outOfBoundsTop() || outOfBoundsBottom())
        Some(Color.RED) else Some(Color.GREEN)
    })
  }

  val fieldWidth = panel.width.map(_ - 25)
  val fieldHeight = panel.height.map(_ - 25)

  override def makeShapes() = {
    val bouncingCircle = new BouncingCircle(Var(50), panel.Mouse.middleButton.pressed)
    val boundingBox = new BoundingBox(fieldWidth, fieldHeight, bouncingCircle.shape)

    List(bouncingCircle.shape, boundingBox.shape)
  }
}
