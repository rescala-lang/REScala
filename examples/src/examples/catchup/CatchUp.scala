package examples.catchup

import examples.catchup._
import react.events.ImperativeEvent
import react.SignalSynt
import react.Var
import react.Signal
import macro.SignalMacro.{ SignalM => Signal }
import swing.{ Panel, MainFrame, SimpleSwingApplication }
import java.awt.{ Color, Graphics2D, Dimension }
import java.awt.Point
import scala.swing.Swing
import scala.swing.event._
import java.awt.Font
import java.awt.Rectangle


object CatchUpStarter {
  def main(args: Array[String]) {
     /* Uncomment to enable logging: */
	//react.ReactiveEngine.log.enableAllLogging
	
    val app = new CatchUp
    app.main(args)
    while (true) {
      Thread sleep 50
      app.tick()
    }
  }
}

class CatchUp extends SimpleSwingApplication {
  
  val Max_X = 800
  val Max_Y = 700
  val Range = 100
  
  val SizeCatch = 100
  val SizeUp = 40
  val SizeY = 32
  

  val tick = new ImperativeEvent[Unit]  
  val time = tick.iterate(0.0){ acc: Double => (acc + 0.1) % (math.Pi * 2)}  
  
  val mouse = new Mouse
  val mouseX = Signal { mouse.position().getX.toInt }
  val mouseY = Signal { mouse.position().getY.toInt }
  
  val xOffset = Signal { math.sin(time()) * Range }
  val yOffset = Signal { math.cos(time()) * Range }
  
  val x = Signal { mouseX() + xOffset().toInt }
  val y = Signal { mouseY() + yOffset().toInt }
  
  val mouseDelayed = mouse.position.delay(20)
  val delayedX = Signal { mouseDelayed().getX.toInt }
  val delayedY = Signal { mouseDelayed().getY.toInt }
  
  val catchBox = Signal { new Rectangle(x(), y(), SizeCatch, SizeY) }
  val upBox = Signal { new Rectangle(delayedX(), delayedY(), SizeUp, SizeY) }
  
  val caught = Signal { catchBox().intersects(upBox()) }  
  val hits = caught.changedTo(true)
  val numberOfHits = hits.iterate(0)(_ + 1)
  
  val scoreString = Signal { "You caught up " + numberOfHits() + " times."}
  
  // redraw code
  val stateChanged = mouse.position.changed || tick  
  stateChanged += { _ => frame.repaint() }

  // drawing code
  def top = frame
  val frame: MainFrame = new MainFrame {
    title = "Catch up!"
    resizable = false
    contents = new Panel() {
      listenTo(mouse.moves, mouse.clicks)

      /** forward mouse events to EScala wrapper class. Should be replaced once reactive GUI lib is complete */
      reactions += {
        case e: MouseMoved => { CatchUp.this.mouse.mouseMovedE(e.point) }
        case e: MousePressed => CatchUp.this.mouse.mousePressedE(e.point)
        case e: MouseDragged => { CatchUp.this.mouse.mouseDraggedE(e.point) }
        case e: MouseReleased => CatchUp.this.mouse.mouseReleasedE(e.point)
      }

      preferredSize = new Dimension(Max_X, Max_Y)
      val myFont = new Font("Tahoma", java.awt.Font.PLAIN, SizeY)
      override def paintComponent(g: Graphics2D) {
        val fontMetrics = g.getFontMetrics(myFont)
        g.setColor(java.awt.Color.DARK_GRAY)        
        g.fill(catchBox.getValue)
        if(caught.getValue)
          g.setColor(java.awt.Color.RED)
        g.fill(upBox.getValue)
        g.setColor(java.awt.Color.WHITE)
        g.setFont(myFont)
        g.drawString("CATCH", catchBox.getValue.getX.toInt, catchBox.getValue.getY.toInt + SizeY - 5)
        g.drawString("UP", upBox.getValue.getX.toInt, upBox.getValue.getY.toInt + SizeY - 5)
        
        g.setColor(new Color(200, 100, 50))
        g.drawString(scoreString.getVal, Max_X / 2 - 100, 40)
      }
    }
  }
}
