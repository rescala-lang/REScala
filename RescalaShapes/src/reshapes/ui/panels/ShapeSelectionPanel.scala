package reshapes.ui.panels

import scala.swing._
import scala.swing.event._
import reshapes.drawing.DrawingSpaceState
import reshapes.figures._
import reshapes._

/**
 * Panel for selection of shapes to draw.
 */
class ShapeSelectionPanel() extends BoxPanel(Orientation.Vertical) {

  val lineBtn = new Button { text = "Line" }
  val rectBtn = new Button { text = "Rectangle" }
  val ovalBtn = new Button { text = "Oval" }
  val triangleBtn = new Button { text = "Triangle" }
  val freedrawBtn = new Button { text = "Freedraw" }

  contents += lineBtn
  contents += rectBtn
  contents += ovalBtn
  contents += triangleBtn
  contents += freedrawBtn

  // reactions
  listenTo(lineBtn)
  listenTo(rectBtn)
  listenTo(ovalBtn)
  listenTo(triangleBtn)
  listenTo(freedrawBtn)

  reactions += {
    case ButtonClicked(`lineBtn`) =>
      Reshapes.currentEvents.nextShape = new Line(Reshapes.currentEvents)
    case ButtonClicked(`rectBtn`) =>
      Reshapes.currentEvents.nextShape = new figures.Rectangle(Reshapes.currentEvents)
    case ButtonClicked(`ovalBtn`) =>
      Reshapes.currentEvents.nextShape = new Oval(Reshapes.currentEvents)
    case ButtonClicked(`triangleBtn`) =>
      Reshapes.currentEvents.nextShape = new Triangle(Reshapes.currentEvents)
    case ButtonClicked(`freedrawBtn`) =>
      Reshapes.currentEvents.nextShape = new Freedraw(Reshapes.currentEvents)
  }
}