package reshapes.versions.signal

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
import reshapes.figures.Shape
import java.awt.Color
import scala.events.behaviour.Signal
import reshapes.EditingMode

trait DrawingSpaceStateInteraction extends DrawingSpaceState {

  val modeChangeOnNextShape: Signal[EditingMode] = Signal {
    nextShape()
    Drawing()
  }
  modeChangeOnNextShape.changed += { newMode =>
    mode = newMode
  }

  val modeChangeOnSelectedShape: Signal[EditingMode] = Signal {
    if (selectedShape() != null) Selection()
    else Drawing()
  }
  modeChangeOnSelectedShape.changed += { newMode =>
    mode = newMode
  }

  val nextShapeSignal: Signal[Shape] = Signal {
    nextShape()
  }
  nextShapeSignal.changed += { shape =>
    allShapes.getValue map (x => x.selected = false)
  }

  val selectedShapeSignal: Signal[Shape] = Signal {
    selectedShape()
  }
  selectedShapeSignal.changed += { shape =>
    allShapes.getValue map (x => x.selected = false)
    if (shape != null) {
      shape.selected = true
    }
  }

  val strokeWidthSignal: Signal[Int] = Signal {
    strokeWidth()
  }
  strokeWidthSignal.changed += { width =>
    if (selectedShape.getValue != null) {
      selectedShape.getValue.strokeWidth = width
    }
  }

  val colorSignal: Signal[Color] = Signal {
    color()
  }
  colorSignal.changed += { color =>
    if (selectedShape.getValue != null) {
      selectedShape.getValue.color = color
    }
  }
}

trait NetworkSpaceStateInteraction extends NetworkSpaceState {

  val sendUpdateSignal: Signal[List[Shape]] = Signal {
    Commands()
    allShapes.getValue
  }

  sendUpdateSignal.changed += { shapes =>
    val socket = new Socket(serverInetAddress, exchangePort)
    val out = new ObjectOutputStream(new DataOutputStream(socket.getOutputStream()))

    out.writeObject(new TransportObject(shapes, listenerPort))

    out.close()
    socket.close()
  }
}

trait CommandPanelInteraction extends CommandPanel {

  val updateSignal: Signal[List[Command]] = Signal {
    Reshapes.CurrentEvents().Commands()
  }

  updateSignal.changed += updateList
}

trait InfoPanelInteraction extends InfoPanel {

  val nextShape: Signal[String] = Signal {
    Reshapes.CurrentEvents().nextShape()
    if (Reshapes.CurrentEvents().nextShape() != null) {
      "next shape: %s".format(Reshapes.CurrentEvents().nextShape().toString())
    } else {
      ""
    }
  }

  val selectedShape: Signal[String] = Signal {
    Reshapes.CurrentEvents().selectedShape()
    if (Reshapes.CurrentEvents().selectedShape() != null) {
      "selected: %s".format(Reshapes.CurrentEvents().selectedShape().toString())
    } else {
      "selected: [none]"
    }
  }

  val numberElements: Signal[String] = Signal {
    "#elements: %d".format(Reshapes.CurrentEvents().allShapes().size)
  }

  val currentStrokeWidth: Signal[String] = Signal {
    "stroke width: %d".format(Reshapes.CurrentEvents().strokeWidth())
  }

  val currentColor: Signal[String] = Signal {
    val color = Reshapes.CurrentEvents().color()
    "color: %s-%s-%s".format(color.getRed(), color.getGreen(), color.getBlue())
  }

  val infoText: Signal[String] = Signal {
    "%s | %s | %s | %s | %s".format(numberElements(), currentColor(), currentStrokeWidth(), nextShape(), selectedShape())
  }

  infoText.changed += (newText => centerLabel.text = newText)
}

trait ShapePanelInteraction extends ShapePanel {

  val allShapesChangedSignal: Signal[List[Shape]] = Signal {
    Reshapes.CurrentEvents().allShapes()
  }

  allShapesChangedSignal.changed += updateAllShapesPanel
}