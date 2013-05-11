package reshapes.ui.panels

import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Orientation
import scala.swing.event.ButtonClicked

import reshapes.Reshapes
import reshapes.drawing.DrawingSpaceState
import reshapes.figures.Freedraw
import reshapes.figures.Line
import reshapes.figures.Oval
import reshapes.figures.Rectangle
import reshapes.figures.Triangle

/**
 * Panel for selection of shapes to draw
 */
class ShapeSelectionPanel extends BoxPanel(Orientation.Vertical) {
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
  
  listenTo(lineBtn, rectBtn, ovalBtn, triangleBtn, freedrawBtn)
  
  reactions += {
    case ButtonClicked(`lineBtn`) =>
      Reshapes.drawingSpaceState.nextShape = new Line(Reshapes.drawingSpaceState)
    case ButtonClicked(`rectBtn`) =>
      Reshapes.drawingSpaceState.nextShape = new Rectangle(Reshapes.drawingSpaceState)
    case ButtonClicked(`ovalBtn`) =>
      Reshapes.drawingSpaceState.nextShape = new Oval(Reshapes.drawingSpaceState)
    case ButtonClicked(`triangleBtn`) =>
      Reshapes.drawingSpaceState.nextShape = new Triangle(Reshapes.drawingSpaceState)
    case ButtonClicked(`freedrawBtn`) =>
      Reshapes.drawingSpaceState.nextShape = new Freedraw(Reshapes.drawingSpaceState)
  }
}