package reshapes

import scala.swing._
import scala.events.scalareact
import scala.events.behaviour.Var

object Reshapes extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "ReShapes";

    val rectBtn = new Button { text = "Rectangle" }
    val circleBtn = new Button { text = "Circle" }

    val toolbox = new BoxPanel(Orientation.Horizontal) {
      contents += rectBtn
      contents += circleBtn
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += toolbox
    }
  }
}