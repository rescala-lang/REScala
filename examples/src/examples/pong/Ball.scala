package examples.pong

import react.events.ImperativeEvent
import java.awt.Point
import react.Signal
import react.SignalSynt
import macro.SignalMacro.{SignalM => Signal}

class Ball(val tick: ImperativeEvent[Unit], val leftRacket: Signal[(Int, Int)], val rightRacket: Signal[(Int, Int)]) {
  import react.conversions.SignalConversions._
  
  val Size = 20
  val Max_X = 800
  val Max_Y = 400
  
  val initPosition = new Point(20, 10)
  val speed = new Point(10,8)
  
  val x: Signal[Int] = tick.fold(initPosition.x) {(pos, _) => pos + speedX.getVal}
  val y: Signal[Int] = tick.fold(initPosition.y) {(pos, _) => pos + speedY.getVal}
  
  
  val leftWall = x.changed && (x => x < 0)
  val rightWall = x.changed && (x => x + Size > Max_X)
  
  val xBounce = leftWall || rightWall
  val yBounce = y.changed && (y => y < 0 || y + Size > Max_Y)
  
  val speedX = xBounce.toggle(speed.x, - speed.x)
  val speedY = yBounce.toggle(speed.y, - speed.y)
}
