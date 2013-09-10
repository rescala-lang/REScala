package exercise

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

object Ball {
  val Size = 20
}

class Pong(val tick: ImperativeEvent[Unit], val mouse: Mouse) {
  import react.conversions.SignalConversions._
  
  val LeftRacketPos = 30
  val RightRacketPos = 770  
  val initPosition = new Point(400, 200)
  val speed = new Point(10,8)
  
  
  val x: Signal[Int] = tick.fold(initPosition.x) {(pos, _) => pos + speedX.getVal}
  val y: Signal[Int] = tick.fold(initPosition.y) {(pos, _) => pos + speedY.getVal}

 
  val leftRacket = new Racket(LeftRacketPos, Signal { 100 })    // The user should control the left racket
  val rightRacket = new Racket(RightRacketPos, Signal { 200 })  // The computer should control the right racket
  
  val rackets = Signal { List(leftRacket, rightRacket) }
  val ballInRacket = ???  // A signal which should be true if the ball is in either racket
  val collisionRacket = ??? // An event which triggers when the ball hits a racket. Hint: use changedTo
  
  val leftWall = x.changed && (x => x < 0)
  val rightWall = x.changed && (x => x + Ball.Size > Pong.Max_X)
  
  val xBounce = leftWall || rightWall
  val yBounce = y.changed && (y => y < 0 || y + Ball.Size > Pong.Max_Y)
  
  val speedX = xBounce.toggle(speed.x, - speed.x)
  val speedY = yBounce.toggle(speed.y, - speed.y)
  
  val pointsPlayer: Signal[Int] = ???  // The points of the player
  val pointsComputer: Signal[Int] = ??? // The points of the computer
  
  val score = Signal {pointsPlayer() + " : " + pointsComputer()}
}

object Racket {
  	val Height = 80
	val Width = 10
}

class Racket(val xPos: Int, val yPos: Signal[Int]) {
  
 val boundedYPos = Signal{ 
    math.min(Pong.Max_Y - Racket.Height / 2, 
    math.max(Racket.Height / 2,
       yPos())) 
   }
	
  val area = Signal { 
	  new Rectangle(xPos - Racket.Width / 2, 
	      boundedYPos() - Racket.Height / 2,
	      Racket.Width,
	      Racket.Height) }
}