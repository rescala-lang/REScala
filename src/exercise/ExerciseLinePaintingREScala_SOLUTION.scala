
package exercise

import scala.collection.mutable.ListBuffer
import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}
import scala.swing.event._
import java.awt.{Color, Graphics2D, Point}
import react.events._
import react._
import macro.SignalMacro.{SignalM => Signal}

/**
 * Dragging the mouse draws a simple graph
 * 
 * @author Frank Teubler, Ingo Maier
 */


object ExercisePaintingREScala_SOLUTION extends SimpleSwingApplication {
  
  
  val base: Var[Double] = Var(0)
  val time = Signal{base()}
  
  val ui = new Panel {
    background = Color.white
    preferredSize = (1000,500)
    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)
    
    val toDraw = ListBuffer[Function1[Graphics2D,Unit]]()
    
    /* EScala events */
    val mouseMovedE = new ImperativeEvent[Point]()
    val mousePressedE = new ImperativeEvent[Point]()
    val mouseDraggedE = new ImperativeEvent[Point]()
    val mouseReleasedE = new ImperativeEvent[Point]()
    val cKeyTypedE = new ImperativeEvent[Unit]()
    
    /* Bind the EScala events to the Swing events */
    reactions += {
      case e: MouseMoved  => { mouseMovedE(e.point) }
      case e: MousePressed  => mousePressedE(e.point)
      case e: MouseDragged  => { mouseDraggedE(e.point) }
      case e: MouseReleased => mouseReleasedE(e.point)
      case KeyTyped(_,'c',_,_) => cKeyTypedE()
    }

    /* A signal-enabled Oval */
    class Oval(center: Signal[Point], radius: Signal[Int]) {
      toDraw += ((g: Graphics2D) =>
        { g.fillOval(center.getVal.x, center.getVal.y, radius.getVal, radius.getVal) })

      override def toString = "Circle(" + center + "," + radius + ")"
    }
    
    /* Compose reactive values */
    val mouseChangePosition = mouseMovedE || mouseDraggedE
    val mousePressedOrReleased = mousePressedE || mouseReleasedE
    val mousePosMoving = mouseChangePosition.latest(new Point(0, 0))
    val pressed = mousePressedOrReleased.toggle(Signal{false}, Signal{true})
    
    
    
 
    val SPEED = 10
    val RANGE = 100
    val changing = Signal{time() / SPEED }
    val xVal = Signal{ Math.sin(changing()) * RANGE } 
    val yVal = Signal{ Math.cos(changing()) * RANGE } 
 
    val center = Signal { 
      if (pressed())
        new Point(mousePosMoving().x + xVal().asInstanceOf[Int], 
                  mousePosMoving().y + yVal().asInstanceOf[Int]) 
      else
        new Point(40 + mousePosMoving().x, 40 + mousePosMoving().y)
    }
    new Oval(center, Signal{20})
    

    

    
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      //g.setColor(new Color(100,100,100))
      toDraw.map(x => x(g))
      //g.setColor(Color.black)
      //repaint()
    }
  }
  
  val top = new MainFrame {
    title = "Simple Line Painting Demo"
    contents = ui
  }
  
  override def main(args: Array[String]){
    super.main(args)
    while (true) {
      base()= base.getVal + 1
      Thread sleep 20
      top.repaint()
    }
  }
}
