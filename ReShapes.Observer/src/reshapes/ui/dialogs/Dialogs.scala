package reshapes.ui.dialogs

import scala.swing.Action
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.CheckBox
import scala.swing.Dialog
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.TextField
import java.awt.Point

abstract class CustomDialog extends Dialog {
  private var dialogResult = false
  
  modal = true;
  
  def showDialog(position: Point = null) = {
    if (position != null)
      location = position
    dialogResult = false
    visible = true
    dialogResult
  }
  
  def hideDialog(result: Boolean) {
    dialogResult = result
    visible = false
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
        applyPorts
        hideDialog(true)
      })
      contents += new Button(Action("Cancel") { hideDialog(false) })
    }
  }
  
  def applyPorts() {
    try {
      hostname = hostnameInput.text
      commandPort = commandPortInput.text.toInt
      exchangePort = exchangePortInput.text.toInt
      listenerPort = listenerPortInput.text.toInt
    }
    catch {
      case e: NumberFormatException =>
        hostname = null
        commandPort = -1
        exchangePort = -1
        listenerPort = -1
        e.printStackTrace
    }
  }
  
  def inputIsValid() =
    hostname != null &&
    hostname.length > 0 &&
    commandPort > 0 &&
    exchangePort > 0 &&
    listenerPort > 0
}

class NewTabDialog extends CustomDialog {
  val showIntersections = new CheckBox("show intersections")
  val showCoordinates = new CheckBox("show coordinates")
  val showNames = new CheckBox("show shape names")
  
  contents = new BoxPanel(Orientation.Vertical) {
    contents += showIntersections
    contents += showCoordinates
    contents += showNames
    contents += new Button(Action("OK") { hideDialog(true) })
  }
}
