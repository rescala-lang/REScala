package examples.demo.gameobjects

import examples.demo.ui.{Rectangle, Shape}
import rescala._

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
