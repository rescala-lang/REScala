package examples.fisheye

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
import java.awt.Point

object FisheyeStarter {
  def main(args: Array[String]) {
    val app = new Fisheye
    app.main(args)
  }
}

object Box {
  	val NormalSize = 50
	val HoverSize = 80 
	val DeltaSize = HoverSize - NormalSize
	val YPos = 20
	val Margin = 10
}

class Box(val color: java.awt.Color, val xOffset: Signal[Int])
	(implicit val mouse: Mouse) {
  
  private def interpolation(d: Double) =  math.max(0, math.min(1, 5 - math.log(d)))   
  
  val lowerLeft = Signal { new java.awt.Point(xOffset() + Box.Margin, Box.YPos)}
  val mouseDistance = Signal { mouse.position().distance(lowerLeft()) }  
  val effectiveSize = Signal { (Box.NormalSize + interpolation(mouseDistance()) * Box.DeltaSize).toInt }
  val rightmostPoint = Signal { lowerLeft().getX.toInt + effectiveSize() }
  
  val area = Signal { 
	  new Rectangle(xOffset(), 
	      Box.YPos,
	      effectiveSize(),
	      effectiveSize())
  }
}

class Fisheye extends SimpleSwingApplication {
  
  val Max_X = 500
  val Max_Y = 200
  val initPoint = Signal { 30 }
  
  implicit val mouse = new Mouse
  
  var boxes: List[Box] = Nil
  def addBox(color: java.awt.Color) {
    boxes ::= new Box(color, if(boxes.isEmpty) initPoint else boxes.head.rightmostPoint)
  }
  
  addBox(Color.BLUE)
  addBox(Color.CYAN)
  addBox(Color.GREEN)
  addBox(Color.YELLOW)
  addBox(Color.RED)
   
  // redraw code
  mouse.position.changed += { _ => frame.repaint() }

  // drawing code
  def top = frame
  val frame: MainFrame = new MainFrame {
    title = "Fisheye boxes"
    resizable = false
    contents = new Panel() {
      
      /** forward mouse events to EScala wrapper class. Should be replaced once reactive GUI lib is complete */
      listenTo(mouse.moves, mouse.clicks)
      reactions += Fisheye.this.mouse.react

      preferredSize = new Dimension(Max_X, Max_Y)
      val scoreFont = new Font("Tahoma", java.awt.Font.PLAIN, 32)
      override def paintComponent(g: Graphics2D) {        
        for(box <- boxes){
          g.setColor(box.color)
          g.fill(box.area.getVal)
        }
      }
    }
  }
}
