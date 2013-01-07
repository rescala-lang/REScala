package reshapes

import scala.swing._
import scala.events.scalareact
import scala.events.behaviour.Var
import util.Random
import scala.swing.event._
import reshapes.figures._
import events.ImperativeEvent
import scala.events.behaviour.Signal

object Reshapes extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "ReShapes";
    preferredSize = new Dimension(1000, 500)

    // Events
    val newShapeSelected = new ImperativeEvent[Drawable]

    // GUI Elements and Layout
    val lineBtn = new Button { text = "Line" }
    val rectBtn = new Button { text = "Rectangle" }
    val circleBtn = new Button { text = "Circle" }

    val drawPanel = new DrawingPanel(newShapeSelected)

    val toolbox = new BoxPanel(Orientation.Horizontal) {
      contents += lineBtn
      contents += rectBtn
      contents += circleBtn
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += toolbox
      contents += drawPanel
    }

    // reactions
    listenTo(lineBtn)
    listenTo(rectBtn)
    listenTo(circleBtn)

    reactions += {
      case ButtonClicked(`lineBtn`) =>
        newShapeSelected(new Line())
        println("line click")
      case ButtonClicked(`rectBtn`) =>
        newShapeSelected(new figures.Rectangle())
        println("rect click")
      case ButtonClicked(`circleBtn`) =>
        println("circle click")
    }
  }
}