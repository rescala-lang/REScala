package examples.demo

import java.awt.Color

import examples.demo.ui.Circle
import rescala._

class BouncingBall {

}

class MovingBall (initX: Int, initY: Int, velocityX: Signal[Int], velocityY: Signal[Int], physicsClock: Event[Int]) {
  val tickX = Event { physicsClock().map(_ * velocityX()) }
  val tickY = Event { physicsClock().map(_ * velocityY()) }
  val posX = tickX.fold(initX)(_ + _)
  val posY = tickY.fold(initY)(_ + _)

  val circle = new Circle(posX, posY, Var(25), Var(Some(Color.BLACK)), Var(None))
}
