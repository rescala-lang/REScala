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

object FoldVersion extends SimpleSwingApplication {
  lazy val application = new FoldVersion
  def top = application.frame
  
  override def main(args: Array[String]) {
    super.main(args)
    while (true) {
	  Swing onEDTWait { application.tick() }
      Thread sleep 20
    }
  }
}

class FoldVersion {
  val Size = 50
  val Max_X = 600
  val Max_Y = 600
  val initPosition = new Point(20, 10)
  val speed = new Point(10,8)
  
  val tick = new ImperativeEvent[Unit]
  
    
  // Implementing switch with fold
  val xx : Signal[Int] = tick.fold(initPosition.x) 
  	{(pos, _) => pos + (if(xSwitch()) speed.x else -speed.x)}
 
  val yy : Signal[Int] = tick.fold(initPosition.y) 
  	{(pos, _) => pos + (if(ySwitch()) speed.y else -speed.y)}

  val xSwitch = (xx.changed && (x => x < 0 || x + Size > Max_X)).fold(false){(a, _) => ! a}
  val ySwitch = (yy.changed && (y => y < 0 || y + Size > Max_Y)).fold(false){(a, _) => ! a}

  
  tick += {_: Unit => frame.repaint()}
  
  // drawing code
  val frame = new MainFrame {
    contents = new Panel() {
      preferredSize = new Dimension(600, 600)
      override def paintComponent(g: Graphics2D) {
	    g.fillOval(xx.getVal, yy.getVal, Size, Size)
      }
    }    
  }
}



