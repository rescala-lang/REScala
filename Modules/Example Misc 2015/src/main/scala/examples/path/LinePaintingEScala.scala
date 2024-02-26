package examples.path

import java.awt.{Color, Graphics2D, Point, geom}

import reactives.default._

import scala.swing.Swing._
import scala.swing.event._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}

object LinePaintingEScala extends SimpleSwingApplication {

  lazy val ui = new Panel {
    background = Color.white
    preferredSize = (1000, 500)
    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)

    /* EScala events */
    val mousePressed  = Evt[Point]()
    val mouseDragged  = Evt[Point]()
    val mouseReleased = Evt[Point]()
    val cKeyTyped     = Evt[Unit]()
    val focusLost     = Evt[Unit]()
    /* Bind the EScala events to the Swing events */
    reactions += {
      case e: MousePressed        => mousePressed.fire(e.point)
      case e: MouseDragged        => mouseDragged.fire(e.point)
      case e: MouseReleased       => mouseReleased.fire(e.point)
      case KeyTyped(_, 'c', _, _) => cKeyTyped.fire()
      case _: FocusLost           => focusLost.fire()
    }

    /* Attach handlers to the EScala events */
    mousePressed observe { p =>
      moveTo(p); requestFocusInWindow(); ()
    }
    mouseDragged observe { lineTo(_) }
    mouseReleased observe { lineTo(_) }
    cKeyTyped observe { _ =>
      path = new geom.GeneralPath; repaint()
    }
    focusLost observe { _ => repaint() }

    /* records the dragging */
    var path = new geom.GeneralPath

    def lineTo(p: Point): Unit = { path.lineTo(p.x.toFloat, p.y.toFloat); repaint() }
    def moveTo(p: Point): Unit = { path.moveTo(p.x.toFloat, p.y.toFloat); repaint() }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.setColor(new Color(100, 100, 100))
      g.drawString(
        "Press left mouse button and drag to paint." +
        (if (hasFocus) " Press 'c' to clear." else ""),
        10,
        size.height - 10
      )
      g.setColor(Color.black)
      g.draw(path)
    }
  }

  def top =
    new MainFrame {
      title = "Simple Line Painting Demo"
      contents = ui
    }
}
