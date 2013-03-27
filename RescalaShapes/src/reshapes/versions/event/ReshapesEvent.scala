package reshapes.versions.event
import java.io.DataOutputStream
import java.io.ObjectOutputStream
import java.net.Socket
import scala.annotation.serializable
import reshapes.network.TransportObject
import reshapes.ui.panels._
import reshapes.Drawing
import reshapes.DrawingSpaceState
import reshapes.NetworkSpaceState
import reshapes.Reshapes
import reshapes.Selection
import scala.events.ImperativeEvent
import reshapes.command.Command
import scala.events.Event

trait DrawingSpaceStateInteraction extends DrawingSpaceState {

  val modeChange = nextShape.changed || selectedShape.changed

  val canvasChange = selectedShape.changed || allShapes.changed || modeChange || strokeWidth.changed || color.changed

  nextShape.changed += (shape => {
    shape.strokeWidth = strokeWidth.getValue
    shape.color = color.getValue
    allShapes.getValue map (x => x.selected = false)
    mode = Drawing()
  })

  selectedShape.changed += (shape => {
    allShapes.getValue map (x => x.selected = false)
    if (shape != null) {
      shape.selected = true
      mode = Selection()
    } else {
      mode = Drawing()
    }
  })

  strokeWidth.changed += (width => {
    if (selectedShape.getValue != null) {
      selectedShape.getValue.strokeWidth = width
    }
  })

  color.changed += (newColor => {
    if (selectedShape.getValue != null) {
      selectedShape.getValue.color = newColor
    }
  })
}

trait NetworkSpaceStateInteraction extends NetworkSpaceState {

  Commands.changed += { _ =>
    val socket = new Socket(serverInetAddress, exchangePort)
    val out = new ObjectOutputStream(new DataOutputStream(socket.getOutputStream()))

    out.writeObject(new TransportObject(allShapes.getValue, listenerPort))

    out.close()
    socket.close()
  }
}

trait CommandPanelInteraction extends CommandPanel {
  var currentState: DrawingSpaceStateInteraction = null

  Reshapes.CurrentEvents.changed += { state =>
    if (currentState != null) currentState.Commands.changed -= updateList
    currentState = state
    currentState.Commands.changed += updateList
  }
}

trait DrawingPanelInteraction extends DrawingPanel {
  //event.canvasChange += (_ => repaint())
}