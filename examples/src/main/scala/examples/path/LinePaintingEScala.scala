
package examples.path

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}
import scala.swing.event._
import java.awt.{Color, Dimension, Graphics, Graphics2D, Point, geom}
import rescala.events._



object LinePaintingEScala extends SimpleSwingApplication {
  
  lazy val ui = new Panel {
    background = Color.white
    preferredSize = (1000,500)
    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)
    
    /* EScala events */
    val mousePressed = new ImperativeEvent[Point]()
    val mouseDragged = new ImperativeEvent[Point]()
    val mouseReleased = new ImperativeEvent[Point]()
    val cKeyTyped = new ImperativeEvent[Unit]()
    val focusLost = new ImperativeEvent[Unit]()
    
    /* Bind the EScala events to the Swing events */
    reactions += {
      case e: MousePressed  => mousePressed(e.point)
      case e: MouseDragged  => mouseDragged(e.point)
      case e: MouseReleased => mouseReleased(e.point)
      case KeyTyped(_,'c',_,_) => cKeyTyped()
      case _: FocusLost => focusLost()
    }
    
    /* Attach handlers to the EScala events */
    mousePressed += {p => moveTo(p); requestFocusInWindow()}
    mouseDragged += { lineTo(_) }
    mouseReleased += {lineTo(_) }
    cKeyTyped += { _ => path = new geom.GeneralPath; repaint() }
    focusLost += { x => repaint() }
    
     

    /* records the dragging */
    var path = new geom.GeneralPath

    def lineTo(p: Point) { path.lineTo(p.x, p.y); repaint() }
    def moveTo(p: Point) { path.moveTo(p.x, p.y); repaint() }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.setColor(new Color(100,100,100))
      g.drawString("Press left mouse button and drag to paint." + 
                   (if(hasFocus) " Press 'c' to clear." else ""), 10, size.height-10)
      g.setColor(Color.black)
      g.draw(path)
    }
  }
  
  def top = new MainFrame {
    title = "Simple Line Painting Demo"
    contents = ui
  }
}
