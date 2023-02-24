package reswing.reshapes.ui.panels

import scala.swing.BoxPanel
import scala.swing.Orientation

import rescala.default._
import reswing.reshapes.ReShapes
import reswing.reshapes.figures.Freedraw
import reswing.reshapes.figures.Line
import reswing.reshapes.figures.Oval
import reswing.reshapes.figures.Rectangle
import reswing.reshapes.figures.Shape
import reswing.reshapes.figures.Triangle
import reswing.ReButton

/** Panel for selection of shapes to draw */
class ShapeSelectionPanel extends BoxPanel(Orientation.Vertical) {
  def state = ReShapes.drawingSpaceState.now

  val lineBtn     = new ReButton("Line")      // #EVT
  val rectBtn     = new ReButton("Rectangle") // #EVT
  val ovalBtn     = new ReButton("Oval")      // #EVT
  val triangleBtn = new ReButton("Triangle")  // #EVT
  val freedrawBtn = new ReButton("Freedraw")  // #EVT

  contents += lineBtn
  contents += rectBtn
  contents += ovalBtn
  contents += triangleBtn
  contents += freedrawBtn

  val nextShape: Signal[Shape] = // #SIG
    ((lineBtn.clicked map { (_: Any) => new Line(state) }) ||                                 // #EF //#EF
      (rectBtn.clicked map { (_: Any) => new Rectangle(state) }) ||                           // #EF //#EF
      (ovalBtn.clicked map { (_: Any) => new Oval(state) }) ||                                // #EF //#EF
      (triangleBtn.clicked map { (_: Any) => new Triangle(state) }) ||                        // #EF //#EF
      (freedrawBtn.clicked map { (_: Any) => new Freedraw(state) })) hold { new Line(state) } // #EF //#IF
}
