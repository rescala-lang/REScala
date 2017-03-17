package examples.demo

import java.awt.Color

import examples.demo.ui.{Circle, Clock, Rectangle, Shape}
import rescala._

object EBoundsBouncingCircle extends Main {
  class BouncingCircle(val diameter: Signal[Int], val reset: Event[Point]) {
    val physicsTicks = Clock.time.change.map{ diff => diff.to.get - diff.from.get }

    val angle = Signal{ Clock.time().toDouble / Clock.NanoSecond }

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

  class BoundingBox(val width: Signal[Int], val height: Signal[Int], val boundShape: Shape) {
    val centerBBHorizontalDistance = Signal{ (width() - boundShape.hitboxWidth()) / 2 }
    val centerBBVerticalDistance = Signal{ (height() - boundShape.hitboxHeight()) / 2 }

    val outOfBoundsLeft = Signal{ boundShape.centerX() < -centerBBHorizontalDistance() }
    val outOfBoundsRight = Signal{ boundShape.centerX() > centerBBHorizontalDistance() }
    val outOfBoundsTop = Signal{ boundShape.centerY() < -centerBBVerticalDistance() }
    val outOfBoundsBottom = Signal{ boundShape.centerY() > centerBBVerticalDistance() }

    val movedOutOfBoundsLeft = outOfBoundsLeft.changedTo(true)
    val movedOutOfBoundsRight = outOfBoundsRight.changedTo(true)
    val movedOutOfBoundsHorizontal = movedOutOfBoundsLeft || movedOutOfBoundsRight
    val movedOutOfBoundsVertical = Signal{ outOfBoundsTop() || outOfBoundsBottom() }.changedTo(true)

    val shape = new Rectangle(Var(0), Var(0), width, height, Signal{
      if(outOfBoundsLeft() || outOfBoundsRight() || outOfBoundsTop() || outOfBoundsBottom())
        Some(Color.RED) else Some(Color.GREEN)
    })
  }

  override def shapes() = {
    val fieldWidth = panelWidth.map(_ - 25)
    val fieldHeight = panelHeight.map(_ - 25)

    val bouncingCircle = new BouncingCircle(Var(50), Mouse.middleButton.pressed)
    val boundingBox = new BoundingBox(fieldWidth, fieldHeight, bouncingCircle.shape)

    bouncingCircle.horizontalBounceSources.transform(boundingBox.movedOutOfBoundsHorizontal :: _)
    bouncingCircle.verticalBounceSources.transform(boundingBox.movedOutOfBoundsVertical :: _)

    List(bouncingCircle.shape, boundingBox.shape)
  }
}
