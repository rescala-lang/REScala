package reshapes.ui.panels

import scala.swing.BoxPanel
import scala.swing.Orientation

import rescala.Signal
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
  def state = ReShapes.drawingSpaceState.get
  
  val lineBtn = new ReButton("Line")  //#EVT
  val rectBtn = new ReButton("Rectangle") //#EVT
  val ovalBtn = new ReButton("Oval") //#EVT
  val triangleBtn = new ReButton("Triangle") //#EVT
  val freedrawBtn = new ReButton("Freedraw") //#EVT
  
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