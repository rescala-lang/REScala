package reshapes

import scala.swing._
import scala.events.scalareact
import scala.events.behaviour.Var
import util.Random
import scala.swing.event._
import reshapes.figures.Line

object Reshapes extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "ReShapes";
    preferredSize = new Dimension(1000, 500)

    val lineBtn = new Button { text = "Line" }
    val rectBtn = new Button { text = "Rectangle" }
    val circleBtn = new Button { text = "Circle" }

    val drawPanel = new DrawingPanel()

    val toolbox = new BoxPanel(Orientation.Horizontal) {
      contents += lineBtn
      contents += rectBtn
      contents += circleBtn
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += toolbox
      contents += drawPanel
    }
  }
}