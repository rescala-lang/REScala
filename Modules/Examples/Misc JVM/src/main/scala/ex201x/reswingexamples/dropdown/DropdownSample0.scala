package ex201x.reswingexamples.dropdown

import ex2013reswing.ReTextField
import reactives.default.*

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
  val inputText      = Signal { inputField.text.value }
  val commaSeparated = Signal { if inputText.value == null then Nil else inputText.value.split(",\\s*").toList }

  val dropdown       = new ReDynamicComboBox(options = commaSeparated, selection = -1)
  val selectionIndex = Signal { dropdown.selection.value }
  val validSelection = Signal {
    if commaSeparated.value.indices.contains(selectionIndex.value) then Some(selectionIndex.value) else None
  }

  // select the currently selected item manually
  val currentSelectedItem = Signal { validSelection.value.map(i => commaSeparated.value(i)) }
  val outputString        = Signal { currentSelectedItem.value.getOrElse("Nothing") }
  val outputField         = new ReTextField(text = outputString)

  /* Debug output */
  // commaSeparated.changed observe { a => println(a) }
  // validSelection.changed observe { a => println(a)}
  // outputString.changed observe { a => println(a)}

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
