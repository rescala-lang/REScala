package examples.demo.gameobjects

import java.awt.Color

import examples.demo.ui.{Rectangle, Shape}
import rescala._

class PlayingField(val width: Signal[Int], val height: Signal[Int], val boundShape: Shape) {
  val horizontalHalfDistance = Signal{ (width() - boundShape.hitboxWidth()) / 2 }
  val verticalHalfDistance = Signal{ (height() - boundShape.hitboxHeight()) / 2 }

  val outOfBoundsLeft = Signal{ boundShape.centerX() < -horizontalHalfDistance() }
  val outOfBoundsRight = Signal{ boundShape.centerX() > horizontalHalfDistance() }
  val outOfBoundsTop = Signal{ boundShape.centerY() < -verticalHalfDistance() }
  val outOfBoundsBottom = Signal{ boundShape.centerY() > verticalHalfDistance() }

  val movedOutOfBoundsLeft = outOfBoundsLeft.changedTo(true)
  val movedOutOfBoundsRight = outOfBoundsRight.changedTo(true)
  val movedOutOfBoundsHorizontal = movedOutOfBoundsLeft || movedOutOfBoundsRight
  val movedOutOfBoundsVertical = Signal{ outOfBoundsTop() || outOfBoundsBottom() }.changedTo(true)

  val shape = new Rectangle(Var(0), Var(0), width, height, Signal{
    if(outOfBoundsLeft() || outOfBoundsRight() || outOfBoundsTop() || outOfBoundsBottom())
      Some(Color.RED) else Some(Color.GREEN)
  })
}
