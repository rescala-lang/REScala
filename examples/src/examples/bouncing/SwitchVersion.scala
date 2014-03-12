package examples.bouncing

import react.events._
import react._
import macro.SignalMacro.{SignalM => Signal}
import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Graphics2D, Dimension}
import java.awt.Point
import scala.swing.Swing

object SwitchVersion extends SimpleSwingApplication {
  lazy val application = new SwitchVersion
  def top = application.frame
  
  override def main(args: Array[String]) {
    super.main(args)
    while (true) {
	  Swing onEDTWait { application.tick() }
      Thread sleep 20
    }
  }
}

class SwitchVersion {
  val Size = 50
  val Max_X = 600
  val Max_Y = 600
  val initPosition = new Point(20, 10)
  val speed = new Point(10,8)
  
  val tick = new ImperativeEvent[Unit]
  
  
  // Using switch
  import react.conversions.SignalConversions._
  
  val x: Signal[Int] = tick.fold(initPosition.x) {(pos, _) => pos + speedX.getVal}
  val y: Signal[Int] = tick.fold(initPosition.y) {(pos, _) => pos + speedY.getVal}
  
  val xBounce = x.changed && (x => x < 0 || x + Size > Max_X)
  val yBounce = y.changed && (y => y < 0 || y + Size > Max_Y)
  
  val speedX = Signal {speed.x}.toggle(xBounce) {- speed.x }  
  val speedY = yBounce.toggle(speed.y, - speed.y)
  
  tick += {_: Unit => frame.repaint()}
  
  // drawing code
  val frame = new MainFrame {
    contents = new Panel() {
      preferredSize = new Dimension(600, 600)
      override def paintComponent(g: Graphics2D) {
	    g.fillOval(x.getVal, y.getVal, Size, Size)
      }
    }    
  }
}



