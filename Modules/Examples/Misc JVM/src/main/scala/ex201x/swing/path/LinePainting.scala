package ex201x.swing.path

import java.awt.{Color, Graphics2D, Point, geom}
import scala.swing.Swing.*
import scala.swing.event.*
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}

object LinePainting extends SimpleSwingApplication {

  lazy val ui = new Panel {
    background = Color.white
    preferredSize = (1000, 500)
    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)

    reactions += {
      case e: MousePressed =>
        moveTo(e.point)
        requestFocusInWindow()
        ()
      case e: MouseDragged  => lineTo(e.point)
      case e: MouseReleased => lineTo(e.point)
      case KeyTyped(_, 'c', _, _) =>
        path = new geom.GeneralPath
        repaint()
      case _: FocusLost => repaint()
    }

    /* records the dragging */
    var path = new geom.GeneralPath

    def lineTo(p: Point): Unit = { path.lineTo(p.x.toFloat, p.y.toFloat); repaint() }
    def moveTo(p: Point): Unit = { path.moveTo(p.x.toFloat, p.y.toFloat); repaint() }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.setColor(new Color(100, 100, 100))
      g.drawString(
        "Press left mouse button and drag to paint." +
        (if hasFocus then " Press 'c' to clear." else ""),
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
