package reshapes.ui.panels

import scala.swing._
import scala.swing.event._
import reshapes.Events
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
      Reshapes.CurrentEvents.getValue.nextShape() = new Line
    case ButtonClicked(`rectBtn`) =>
      Reshapes.CurrentEvents.getValue.nextShape() = new figures.Rectangle
    case ButtonClicked(`ovalBtn`) =>
      Reshapes.CurrentEvents.getValue.nextShape() = new Oval
    case ButtonClicked(`triangleBtn`) =>
      Reshapes.CurrentEvents.getValue.nextShape() = new Triangle
    case ButtonClicked(`freedrawBtn`) =>
      Reshapes.CurrentEvents.getValue.nextShape() = new Freedraw
  }
}