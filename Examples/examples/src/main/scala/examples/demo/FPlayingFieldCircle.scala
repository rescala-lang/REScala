package examples.demo

import examples.demo.EModularMouseBouncingCircle.BouncingCircle
import examples.demo.ui._
import rescala._

object FPlayingFieldCircle extends Main {
  class PlayingField(val width: Signal[Int], val height: Signal[Int]) {
    val shape = new Rectangle(Var(0), Var(0), width, height)
  }

  val fieldWidth = panel.width.map(_ - 25)
  val fieldHeight = panel.height.map(_ - 25)

  override def makeShapes() = {
    val bouncingCircle = new BouncingCircle(Var(50), panel.Mouse.middleButton.pressed)
    val playingField = new PlayingField(fieldWidth, fieldHeight)

    bouncingCircle.horizontalBounceSources.transform(panel.Mouse.leftButton.pressed :: _)
    bouncingCircle.verticalBounceSources.transform(panel.Mouse.rightButton.pressed :: _)

    List(bouncingCircle.shape, playingField.shape)
  }
}
