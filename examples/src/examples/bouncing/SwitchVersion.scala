package examples.bouncing

import react.events.ImperativeEvent
import react.SignalSynt
import react.Var
import react.Signal
import macro.SignalMacro.{SignalM => Signal}
import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Graphics2D, Dimension}
import java.awt.Point
import scala.swing.Swing

object SwitchVersionStart {
	def main(args: Array[String]){
	  	/* Uncomment to enable logging: */
		//react.ReactiveEngine.log.enableAllLogging
		val app = new SwitchVersionFrame
		app.main(args)
		while (true) {
			Thread sleep 20
			app.tick()
		}
	}
}

class SwitchVersionFrame extends SimpleSwingApplication {
  val Size = 50
  val Max_X = 600
  val Max_Y = 600
  val initPosition = new Point(20, 10)
  val speed = new Point(10,8)
  
  val tick = new ImperativeEvent[Unit]
  
  
 // VERSION 1: // best, but does not work due to cyclic references!
//   def move(init : Int, translation : Int => Int) : Signal[Int] = 
//    Signal.foldE(tick, init)((x,_) => translation(x))
//  val x : Signal[Int] = move(initPosition.x, (_ + speed.x)).toggle(xBounce, 
//		  				move(initPosition.x, (_ - speed.x)))
//  
//  val y : Signal[Int] = move(initPosition.y, (_ + speed.y)).toggle(yBounce, 
//		  				move(initPosition.y, (_ - speed.y)))
//  val xBounce = x.changed && (x => x < 0 || x + Size > Max_X)
//  val yBounce = y.changed && (y => y < 0 || y + Size > Max_Y)		  				
  
    
  // VERSION 2: implementing switch without library function
//  val x : Signal[Int] = tick.fold(initPosition.x) 
//  	{(pos, _) => pos + (if(xSwitch()) speed.x else -speed.x)}
// 
//  val y : Signal[Int] = tick.fold(initPosition.y) 
//  	{(pos, _) => pos + (if(ySwitch()) speed.y else -speed.y)}
//
//  val xSwitch = Signal.foldE(x.changed && (x => x < 0 || x + Size > Max_X), false) {(a, _) => ! a}
//  val ySwitch = Signal.foldE(y.changed && (y => y < 0 || y + Size > Max_Y), false) {(a, _) => ! a}
  
  
  // VERSION 3: // working, but a little long
  
  import react.conversions.SignalConversions._
  
  val x: Signal[Int] = tick.fold(initPosition.x) {(pos, _) => pos + speedX.getVal}
  val y: Signal[Int] = tick.fold(initPosition.y) {(pos, _) => pos + speedY.getVal}
  
  val xBounce = x.changed && (x => x < 0 || x + Size > Max_X)
  val yBounce = y.changed && (y => y < 0 || y + Size > Max_Y)
  
  val speedX = Signal {speed.x}.toggle(xBounce) {- speed.x }  
  val speedY = yBounce.toggle(speed.y, - speed.y)
  
  tick += {_: Unit => frame.repaint()}
  
  // drawing code
  def top = frame  
  val frame = new MainFrame {
    contents = new Panel() {
      preferredSize = new Dimension(600, 600)
      override def paintComponent(g: Graphics2D) {
	    g.fillOval(x.getVal, y.getVal, Size, Size)
      }
    }    
  }
}



