package reshapes.panels

import scala.swing._
import scala.swing.event._
import reshapes.Events
import reshapes.figures._
import reshapes._

/**
 * Panel for selection of shapes to draw.
 */
class ShapeSelectionPanel(var events: Events) extends BoxPanel(Orientation.Vertical) {

  val lineBtn = new Button { text = "Line" }
  val rectBtn = new Button { text = "Rectangle" }
  val ovalBtn = new Button { text = "Oval" }

  contents += lineBtn
  contents += rectBtn
  contents += ovalBtn

  // reactions
  listenTo(lineBtn)
  listenTo(rectBtn)
  listenTo(ovalBtn)

  reactions += {
    case ButtonClicked(`lineBtn`) =>
      events.nextShape() = new Line
    case ButtonClicked(`rectBtn`) =>
      events.nextShape() = new figures.Rectangle
    case ButtonClicked(`ovalBtn`) =>
      events.nextShape() = new Oval
  }
}