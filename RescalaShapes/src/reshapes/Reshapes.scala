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
    val ovalBtn = new Button { text = "Oval" }

    val drawPanel = new DrawingPanel(newShapeSelected)

    val toolbox = new BoxPanel(Orientation.Horizontal) {
      contents += lineBtn
      contents += rectBtn
      contents += ovalBtn
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += toolbox
      contents += drawPanel
    }

    // reactions
    listenTo(lineBtn)
    listenTo(rectBtn)
    listenTo(ovalBtn)

    reactions += {
      case ButtonClicked(`lineBtn`) =>
        newShapeSelected(new Line())
      case ButtonClicked(`rectBtn`) =>
        newShapeSelected(new figures.Rectangle())
      case ButtonClicked(`ovalBtn`) =>
        newShapeSelected(new Oval())
    }
  }
}