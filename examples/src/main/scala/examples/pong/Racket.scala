package examples.pong

import rescala.Signal
import rescala.Signal
import rescala.SignalSynt
import makro.SignalMacro.{SignalM => Signal}
import java.awt.Rectangle

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