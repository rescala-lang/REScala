package reshapes

import scala.swing._
import scala.events.scalareact
import scala.events.behaviour.Var
import util.Random
import scala.swing.event._
import reshapes.figures._
import events.ImperativeEvent
import scala.events.behaviour.Signal
import java.awt.Color

object Reshapes extends SimpleSwingApplication {
  lazy val ui = new BorderPanel {
    val events = new EventHolder

    // GUI Elements and Layout
    val lineBtn = new Button { text = "Line" }
    val rectBtn = new Button { text = "Rectangle" }
    val ovalBtn = new Button { text = "Oval" }
    val undoBtn = new Button { text = "<"; enabled = false }
    val strokeWidthInput = new TextField { text = events.strokeWidth.getValue.toString(); columns = 5 }
    val colorInput = new TextField { text = "0,0,0"; columns = 10 }
    val shapePanel = new ShapePanel(events)

    add(new FlowPanel {
      contents += undoBtn
      contents += new Label { text = "stroke width: " }
      contents += strokeWidthInput
      contents += new Label { text = "stroke color: " }
      contents += colorInput
    }, BorderPanel.Position.North)

    add(new BoxPanel(Orientation.Vertical) {
      contents += lineBtn
      contents += rectBtn
      contents += ovalBtn
    }, BorderPanel.Position.West)

    add(new DrawingPanel(events), BorderPanel.Position.Center)
    add(new InfoPanel(events), BorderPanel.Position.South)
    add(shapePanel, BorderPanel.Position.East)

    // reactions
    listenTo(lineBtn)
    listenTo(rectBtn)
    listenTo(ovalBtn)
    listenTo(undoBtn)
    listenTo(strokeWidthInput)
    listenTo(colorInput)
    listenTo(mouse.clicks)

    reactions += {
      case ButtonClicked(`lineBtn`) =>
        events.nextShape() = new Line
      case ButtonClicked(`rectBtn`) =>
        events.nextShape() = new figures.Rectangle
      case ButtonClicked(`ovalBtn`) =>
        events.nextShape() = new Oval
      case ButtonClicked(`undoBtn`) =>
        events.Commands.getValue.first.revert()
        events.Commands() = events.Commands.getValue.tail
      case EditDone(`strokeWidthInput`) =>
        try {
          events.strokeWidth() = strokeWidthInput.text.toInt match {
            case i if i > 0 => i
            case _ => strokeWidthInput.text = "1"; 1
          }
        } catch {
          case e: NumberFormatException => strokeWidthInput.text = events.strokeWidth.getValue.toString()
        }
      case EditDone(`colorInput`) =>
        try {
          val input = colorInput.text.split(',') match {
            case empty if empty.length == 1 && empty(0).isEmpty() =>
              events.color() = new Color(0, 0, 0)
              colorInput.text = "0,0,0"
            case rgbStr if rgbStr.length == 3 =>
              val rgb = rgbStr.map(x => x.toInt)
              events.color() = new Color(rgb(0), rgb(1), rgb(2))
            case _ => throw new NumberFormatException
          }
        } catch {
          case _ => colorInput.text = "%d,%d,%d".format(events.color.getValue.getRed(), events.color.getValue.getGreen(), events.color.getValue.getBlue())
        }
    }

    events.Commands.changed += (commands => undoBtn.enabled = commands.size > 0)
  }

  def top = new MainFrame {
    title = "ReShapes";
    preferredSize = new Dimension(1000, 500)

    contents = ui
  }
}