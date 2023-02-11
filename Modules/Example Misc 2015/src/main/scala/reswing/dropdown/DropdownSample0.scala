package reswing.dropdown

import rescala.default._
import reswing.ReTextField._
import reswing._

import scala.swing.{BoxPanel, FlowPanel, Label, MainFrame, Orientation, SimpleSwingApplication}

/*
object DropdownSample {
  def main(args: Array[String]) {
    val window = new CityWindow
    window.pack
    window.visible = true
  }
}*/

object DropdownSample0 extends SimpleSwingApplication {

  val inputField     = new ReTextField(text = "Berlin, Paris, London, Rome", columns = 50)
  val inputText      = Signal { inputField.text() }
  val commaSeparated = Signal { if (inputText() == null) Nil else inputText().split(",\\s*").toList }

  val dropdown       = new ReDynamicComboBox(options = commaSeparated, selection = -1)
  val selectionIndex = Signal { dropdown.selection() }
  val validSelection = Signal {
    if (commaSeparated().indices.contains(selectionIndex())) Some(selectionIndex()) else None
  }

  // select the currently selected item manually
  val currentSelectedItem = Signal { validSelection().map(i => commaSeparated()(i)) }
  val outputString        = Signal { currentSelectedItem().getOrElse("Nothing") }
  val outputField         = new ReTextField(text = outputString)

  /* Debug output */
  // commaSeparated.changed += { a => println(a) }
  // validSelection.changed += { a => println(a)}
  // outputString.changed += { a => println(a)}

  def top =
    new MainFrame {
      title = "Dropdown example 0"
      contents = new BoxPanel(Orientation.Vertical) {

        contents += new FlowPanel {
          contents += new Label("Comma-separated values: ")
          contents += inputField
        }
        contents += new FlowPanel {
          contents += new Label("Dropdown selection: ")
          contents += dropdown
        }

        contents += new FlowPanel {
          contents += new Label("Selected item: ")
          contents += outputField
        }
      }
    }
}
