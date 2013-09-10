package examples.pong

import react.events.ImperativeEvent
import java.awt.Point
import react.Signal
import react.SignalSynt
import macro.SignalMacro.{SignalM => Signal}
import java.awt.Rectangle

object Pong {
  val Max_X = 800
  val Max_Y = 400
}

class Pong(val tick: ImperativeEvent[Unit], val mouse: Mouse) {
  import react.conversions.SignalConversions._
  
  val Size = 20
  
  val LeftRacketPos = 30
  val RightRacketPos = 770
  
  val initPosition = new Point(20, 10)
  val speed = new Point(10,8)
  
  val x: Signal[Int] = tick.fold(initPosition.x) {(pos, _) => pos + speedX.getVal}
  val y: Signal[Int] = tick.fold(initPosition.y) {(pos, _) => pos + speedY.getVal}
  
  
  val mouseY = Signal{ mouse.position().getY().toInt}
 
  val leftRacket = new Racket(LeftRacketPos, mouseY)  
  val rightRacket = new Racket(RightRacketPos, y)
  
  val rackets = Signal { List(leftRacket, rightRacket) }
  val areas = Signal { rackets().map(_.area())}
  val ballInRacket = Signal { areas().exists(_.contains(x(), y()))}
  val collisionRacket = ballInRacket.changedTo(true)
  
  val leftWall = x.changed && (x => x < 0)
  val rightWall = x.changed && (x => x + Size > Pong.Max_X)
  
  val xBounce = leftWall || rightWall || collisionRacket
  val yBounce = y.changed && (y => y < 0 || y + Size > Pong.Max_Y)
  
  val speedX = xBounce.toggle(speed.x, - speed.x)
  val speedY = yBounce.toggle(speed.y, - speed.y)
  
}
