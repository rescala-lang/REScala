package reshapes.ui.panels

import scala.swing.BoxPanel
import scala.swing.Orientation

import react.Signal
import reshapes.ReShapes
import reshapes.figures.Freedraw
import reshapes.figures.Line
import reshapes.figures.Oval
import reshapes.figures.Rectangle
import reshapes.figures.Shape
import reshapes.figures.Triangle
import reswing.ReButton

/**
 * Panel for selection of shapes to draw
 */
class ShapeSelectionPanel extends BoxPanel(Orientation.Vertical) {
  def state = ReShapes.drawingSpaceState.getValue
  
  val lineBtn = ReButton("Line")  //#EVT
  val rectBtn = ReButton("Rectangle") //#EVT
  val ovalBtn = ReButton("Oval") //#EVT
  val triangleBtn = ReButton("Triangle") //#EVT
  val freedrawBtn = ReButton("Freedraw") //#EVT
  
  contents += lineBtn
  contents += rectBtn
  contents += ovalBtn
  contents += triangleBtn
  contents += freedrawBtn
  
  val nextShape: Signal[Shape] = //#SIG
	  ((lineBtn.clicked map {_: Any => new Line(state) }) || //#EF //#EF
	   (rectBtn.clicked map {_: Any => new Rectangle(state) }) || //#EF //#EF
	   (ovalBtn.clicked map {_: Any => new Oval(state) }) || //#EF //#EF
	   (triangleBtn.clicked map {_: Any => new Triangle(state) }) || //#EF //#EF
	   (freedrawBtn.clicked map {_: Any => new Freedraw(state) })) latest { new Line(state) } //#EF //#IF
}