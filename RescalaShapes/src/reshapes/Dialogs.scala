package reshapes
import scala.swing._
import javax.swing.JOptionPane

class ServerDialog extends Dialog {

  val hostnameInput = new TextField(50) { text = "localhost" }
  val commandPortInput = new TextField(10) { text = "9998" }
  val exchangePortInput = new TextField(10) { text = "9999" }
  val listenerPortInput = new TextField(10) { text = "1337" }

  var hostname: String = null
  var commandPort: Int = -1
  var exchangePort: Int = -1
  var listenerPort: Int = -1

  this.modal = true;

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
        applyPorts()
      })
      contents += new Button(Action("Cancel") {
        hideDialog()
      })
    }
  }

  def showDialog() = {
    this.visible = true
  }

  def hideDialog() = {
    this.visible = false
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
    hostname.length() > 0 && commandPort > 0 && exchangePort > 0 && listenerPort > 0
  }
}