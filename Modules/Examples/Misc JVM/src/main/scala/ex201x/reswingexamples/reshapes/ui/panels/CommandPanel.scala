package ex201x.reswingexamples.reshapes.ui.panels

import reactives.default.*
import ex2013reswing.{ReBoxPanel, ReButton}
import ex201x.reswingexamples.reshapes.ReShapes
import ex201x.reswingexamples.reshapes.drawing.Command
import ex201x.reswingexamples.reshapes.util.ReactiveUtil.UnionEvent

import scala.swing.{BoxPanel, Component, Orientation, ScrollPane}

/** The CommandPanel lists all executed commands and makes it possible to revert them */
class CommandPanel extends BoxPanel(Orientation.Vertical) {
  def state = ReShapes.drawingSpaceState

  val commands = Signal.dynamic { if state.value != null then state.value.commands.value else List.empty } // #SIG

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
