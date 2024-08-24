package ex201x.reswingexamples.reshapes.ui.panels

import ex2013reswing.{ReBoxPanel, ReButton}
import ex201x.reswingexamples.reshapes.ReShapes
import ex201x.reswingexamples.reshapes.drawing.{DeleteShape, DrawingSpaceState}
import ex201x.reswingexamples.reshapes.figures.Shape
import ex201x.reswingexamples.reshapes.util.ReactiveUtil.UnionEvent
import reactives.default.*

import scala.swing.{BoxPanel, Color, Component, Label, Orientation, ScrollPane}

/** Lists all drawn shapes */
class ShapePanel extends BoxPanel(Orientation.Vertical) {
  def state = ReShapes.drawingSpaceState

  val shapes = Signal.dynamic { if state.value != null then state.value.shapes.value else List.empty } // #SIG

  val shapeViews = Signal { shapes.value map { shape => new ShapeView(shape, state.value) } } // #SIG

  val shapesPanel = new ReBoxPanel(
    orientation = Orientation.Vertical,
    contents = Signal[Seq[Component]] { // #SIG
      shapeViews.value map { (shapeView: ShapeView) => shapeView: Component }
    }
  )

  contents += new ScrollPane {
    contents = shapesPanel
  }

  val deleted =
    UnionEvent(Signal { shapeViews.value map { shapeView => shapeView.deleted } }) // #SIG //#UE( //#EVT //#IF )
}

class ShapeView(shape: Shape, state: DrawingSpaceState) extends ReBoxPanel(Orientation.Horizontal) {
  val SELECTED_COLOR     = new Color(0, 153, 255)
  val NOT_SELECTED_COLOR = new Color(255, 255, 255)

  val deleteButton = new ReButton("delete")

  val deleted: Event[DeleteShape] = // #EVT
    deleteButton.clicked map { (_: Any) => new DeleteShape(shape) } // #EF

  peer.background = NOT_SELECTED_COLOR
  peer.contents += new Label(shape.toString)
  peer.contents += deleteButton

  mouse.clicks.clicked observe { _ => // #HDL
    state.select.fire(if state.selectedShape.now != shape then shape else null)
  }

  state.selectedShape.changed observe { selected => // #HDL
    peer.background = if selected == shape then SELECTED_COLOR else NOT_SELECTED_COLOR
  }
}
