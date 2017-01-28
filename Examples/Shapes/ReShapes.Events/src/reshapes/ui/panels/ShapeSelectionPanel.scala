package reshapes.ui.panels

import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Orientation
import scala.swing.event.ButtonClicked

import reshapes.ReShapes
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
  val lineBtn = new Button { text = "Line" }  //#EVT
  val rectBtn = new Button { text = "Rectangle" } //#EVT
  val ovalBtn = new Button { text = "Oval" }  //#EVT
  val triangleBtn = new Button { text = "Triangle" }  //#EVT
  val freedrawBtn = new Button { text = "Freedraw" }  //#EVT
  
  contents += lineBtn
  contents += rectBtn
  contents += ovalBtn
  contents += triangleBtn
  contents += freedrawBtn
  
  listenTo(lineBtn, rectBtn, ovalBtn, triangleBtn, freedrawBtn)
  
  reactions += {  //#HDL
    case ButtonClicked(`lineBtn`) =>  
      ReShapes.drawingSpaceState.nextShape = new Line(ReShapes.drawingSpaceState)
    case ButtonClicked(`rectBtn`) =>
      ReShapes.drawingSpaceState.nextShape = new Rectangle(ReShapes.drawingSpaceState)
    case ButtonClicked(`ovalBtn`) =>
      ReShapes.drawingSpaceState.nextShape = new Oval(ReShapes.drawingSpaceState)
    case ButtonClicked(`triangleBtn`) =>
      ReShapes.drawingSpaceState.nextShape = new Triangle(ReShapes.drawingSpaceState)
    case ButtonClicked(`freedrawBtn`) =>
      ReShapes.drawingSpaceState.nextShape = new Freedraw(ReShapes.drawingSpaceState)
  }
}