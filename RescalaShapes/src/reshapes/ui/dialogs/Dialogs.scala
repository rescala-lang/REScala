package reshapes.ui.dialogs
import scala.swing._
import javax.swing.JOptionPane
import reshapes.DrawingSpaceState
import reshapes.ui.panels._
import reshapes.versions.event.DrawingSpaceStateInteraction

object DialogResult extends Enumeration {
  type DialogResult = Value
  val OK, CANCEL = Value
}

abstract class CustomDialog extends Dialog {
  var dialogResult = DialogResult.CANCEL

  this.modal = true;

  def showDialog() = {
    dialogResult = DialogResult.CANCEL
    this.visible = true
  }

  def hideDialog() = {
    this.visible = false
  }
}

class ServerDialog extends CustomDialog {

  val hostnameInput = new TextField(50) { text = "localhost" }
  val commandPortInput = new TextField(10) { text = "9998" }
  val exchangePortInput = new TextField(10) { text = "9999" }
  val listenerPortInput = new TextField(10) { text = "1337" }

  var hostname: String = null
  var commandPort: Int = -1
  var exchangePort: Int = -1
  var listenerPort: Int = -1

  contents = new BoxPanel(Orientation.Vertical) {
    contents += new Label("Server hostname")
    contents += hostnameInput
    contents += new Label("Server Command-Port")
    contents += commandPortInput
    contents += new Label("Server Shapes-Exchange-Port")
    contents += exchangePortInput
    contents += new Label("Client listener port")
    contents += listenerPortInput
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new Button(Action("OK") {
        dialogResult = DialogResult.OK
        applyPorts()
      })
      contents += new Button(Action("Cancel") {
        hideDialog()
      })
    }
  }

  def applyPorts() = {
    hostname = hostnameInput.text
    try {
      commandPort = commandPortInput.text.toInt
      exchangePort = exchangePortInput.text.toInt
      listenerPort = listenerPortInput.text.toInt
    } catch {
      case e: NumberFormatException => e.printStackTrace()
    }
    hideDialog()
  }

  def inputIsValid(): Boolean = {
    hostname != null && hostname.length() > 0 && commandPort > 0 && exchangePort > 0 && listenerPort > 0
  }
}

class NewTabDialog extends CustomDialog {

  val showIntersections = new CheckBox("show intersections")
  val showCoordinates = new CheckBox("show coordinates")
  val showNames = new CheckBox("show shape names")

  contents = new BoxPanel(Orientation.Vertical) {
    contents += showIntersections
    contents += showCoordinates
    contents += showNames
    contents += new Button(Action("OK") {
      dialogResult = DialogResult.OK
      hideDialog()
    })
  }

  /**
   * Creates a custom drawing panel (with different traits) depending on checked dialog options
   */
  def generateDrawingPanel(event: DrawingSpaceStateInteraction): DrawingPanel = {
    val tuple = (showIntersections.selected, showCoordinates.selected, showNames.selected)
    tuple match {
      case (true, false, false) => return new DrawingPanel(event) with ShowIntersection
      case (false, true, false) => return new DrawingPanel(event) with ShowCoordinateSystem
      case (true, true, false) => return new DrawingPanel(event) with ShowIntersection with ShowCoordinateSystem
      case (false, false, true) => return new DrawingPanel(event) with ShowNameLabels
      case (true, false, true) => return new DrawingPanel(event) with ShowIntersection with ShowNameLabels
      case (true, true, true) => return new DrawingPanel(event) with ShowIntersection with ShowCoordinateSystem with ShowNameLabels
      case _ => return new DrawingPanel(event)
    }
  }
}
