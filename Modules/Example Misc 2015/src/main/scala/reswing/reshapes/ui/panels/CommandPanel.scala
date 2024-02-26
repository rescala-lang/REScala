package reswing.reshapes.ui.panels

import scala.swing.BoxPanel
import scala.swing.Component
import scala.swing.Orientation
import scala.swing.ScrollPane

import reactives.default._
import reswing.reshapes.ReShapes
import reswing.reshapes.drawing.Command
import reswing.reshapes.util.ReactiveUtil.UnionEvent
import reswing.ReBoxPanel
import reswing.ReButton

/** The CommandPanel lists all executed commands and makes it possible to revert them */
class CommandPanel extends BoxPanel(Orientation.Vertical) {
  def state = ReShapes.drawingSpaceState

  val commands = Signal.dynamic { if (state.value != null) state.value.commands.value else List.empty } // #SIG

  val buttonsEvents = Signal { // #SIG
    commands.value map { command =>
      val button = new ReButton(command.description) // #IS( //#EVT )
      (button: Component, button.clicked map { (_: Any) => command })
    }
  }

  val revert = UnionEvent(Signal { // #SIG //#UE( //#EVT //#IF )
    buttonsEvents.value map { case (_, ev) => ev: Event[Command] }
  })

  val commandPanel = new ReBoxPanel(
    orientation = Orientation.Vertical,
    contents = Signal { (buttonsEvents.value map { case (btn, _) => btn }): Seq[Component] }
  ) // #SIG //#IS( // )

  contents += new ScrollPane {
    contents = commandPanel
  }
}
