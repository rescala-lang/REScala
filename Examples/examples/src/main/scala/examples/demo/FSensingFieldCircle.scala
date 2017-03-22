package examples.demo

import java.awt.Color

import examples.demo.gameobjects.BouncingCircle
import examples.demo.ui._
import rescala._

object FSensingFieldCircle extends Main {
  class PlayingField(val width: Signal[Int], val height: Signal[Int], val boundShape: Shape) {
    val horizontalHalfDistance = Signal{ (width() - boundShape.hitboxWidth()) / 2 }
    val verticalHalfDistance = Signal{ (height() - boundShape.hitboxHeight()) / 2 }

    val outOfBoundsLeft = Signal{ boundShape.centerX() < -horizontalHalfDistance() }
    val outOfBoundsRight = Signal{ boundShape.centerX() > horizontalHalfDistance() }
    val outOfBoundsTop = Signal{ boundShape.centerY() < -verticalHalfDistance() }
    val outOfBoundsBottom = Signal{ boundShape.centerY() > verticalHalfDistance() }

    val shape = new Rectangle(Var(0), Var(0), width, height, Signal{
      if(outOfBoundsLeft() || outOfBoundsRight() || outOfBoundsTop() || outOfBoundsBottom())
        Some(Color.RED) else Some(Color.GREEN)
    })
  }

  val fieldWidth = panel.width.map(_ - 25)
  val fieldHeight = panel.height.map(_ - 25)

  override def makeShapes() = {
    val bouncingCircle = new BouncingCircle(Var(50), panel.Mouse.middleButton.pressed)
    val playingField = new PlayingField(fieldWidth, fieldHeight, bouncingCircle.shape)

    bouncingCircle.horizontalBounceSources.transform(panel.Mouse.leftButton.pressed :: _)
    bouncingCircle.verticalBounceSources.transform(panel.Mouse.rightButton.pressed :: _)

    List(bouncingCircle.shape, playingField.shape)
  }
}
