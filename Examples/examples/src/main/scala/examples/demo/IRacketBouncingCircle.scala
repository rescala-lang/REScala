package examples.demo

import examples.demo.gameobjects.{BouncingCircle, PlayingField}
import examples.demo.ui._
import rescala._

object IRacketBouncingCircle extends Main {
  class Racket(val fieldWidth: Signal[Int], val fieldHeight: Signal[Int], val height: Signal[Int], val isRight: Boolean, val inputY: Signal[Int]) {
    val width = Var(10)

    val posX = fieldWidth.map(w => (if(isRight) 1 else -1) * (w / 2 - 25))
    val posY = Signal{ math.max(math.min(inputY(), (fieldHeight() - height()) / 2), - (fieldHeight() - height()) / 2) }

    val shape = new Rectangle(posX, posY, width, height)

    def collisionWith(collider: Shape): Event[Unit] = {
      val collisionBoxHeight = Signal{ height() + collider.hitboxHeight() }
      val collisionBoxWidth = Signal{ width() + collider.hitboxWidth() }
      val shapeInsideRacket = Signal{ (posX() - collisionBoxWidth() / 2 < collider.centerX()) &&
        (posX() + collisionBoxWidth() / 2 > collider.centerX()) &&
        (posY() - collisionBoxHeight() / 2 < collider.centerY()) &&
        (posY() + collisionBoxHeight() / 2 > collider.centerY())}
      shapeInsideRacket.changedTo(true)
    }
  }

  val fieldWidth = panel.width.map(_ - 25)
  val fieldHeight = panel.height.map(_ - 25)

  override def makeShapes() = {
    val bouncingCircle = new BouncingCircle(Var(50), panel.Mouse.middleButton.pressed)
    val playingField = new PlayingField(fieldWidth, fieldHeight, bouncingCircle.shape)
    val racket = new Racket(fieldWidth, fieldHeight, Var(100), true, panel.Mouse.y)

    bouncingCircle.horizontalBounceSources.transform(boundingBox.movedOutOfBoundsHorizontal :: racket.collisionWith(bouncingCircle.shape) :: _)
    bouncingCircle.verticalBounceSources.transform(boundingBox.movedOutOfBoundsVertical :: _)

    List(bouncingCircle.shape, playingField.shape, racket.shape)
  }
}
