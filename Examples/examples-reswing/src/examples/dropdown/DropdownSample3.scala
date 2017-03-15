package examples.dropdown

import rescala._
import reswing.ReTextField._
import reswing._

import scala.swing.{BoxPanel, Button, FlowPanel, Label, MainFrame, Orientation, SimpleSwingApplication}

object DropdownSample3 extends SimpleSwingApplication {


  // initial values
  val col1 = new ReTextField(text = "Berlin", columns = 30)
  val col2 = new ReTextField(text = "Paris", columns = 30)
  val val1 = Signal { col1.text() }
  val val2 = Signal { col2.text() }

  val fields: Var[List[Signal[String]]] = Var(List(val1, val2))
  val nFields = Signal { fields().size }

  val listOfSignals = Signal { fields() }
  val options = Signal { listOfSignals().map(_()) }

  val innerChanged = Signal { listOfSignals().map(_.changed) }
  val anyChangedWrapped = Signal { innerChanged().reduce((a, b) => a || b)}
  val anyChanged = anyChangedWrapped.unwrap

  anyChanged += {x => println("some value has changed: " + x)}


  val dropdown = new ReDynamicComboBox (options = options, selection = -1)
  val selectionIndex = Signal { dropdown.selection() }
  val validSelection = Signal { if (options().indices.contains(selectionIndex())) Some(selectionIndex()) else None}

  // select the currently selected item manually
  val currentSelectedItem = Signal { validSelection().map{i => listOfSignals()(i)() }}
  val outputString = Signal { currentSelectedItem().getOrElse("Nothing") }
  val outputField = new ReTextField(text = outputString)

  lazy val frame = new MainFrame {
	  title = "Dropdown example"
	  val fields = new BoxPanel(Orientation.Vertical) {

	    contents += new FlowPanel {
	    	contents += new Label("Dropdown selection: ")
	    	contents += dropdown
	    }

	    contents += new FlowPanel {
	    	contents += new Label("Selected item: ")
	    	contents += outputField
	    }

	    contents += new FlowPanel {
	    	contents += Button("Add value")(addField("empty"))
	    }


	    contents += new FlowPanel {
	      contents += new Label("Value 1:")
	      contents += col1
	    }

	    contents += new FlowPanel {
	      contents += new Label("Value 2:")
	      contents += col2
	    }

	  }
	  contents = fields
  }

  def top = frame

  def addField(initText: String) {
    val n = nFields.get + 1
    val col = new ReTextField(text = initText, columns = 30)
    frame.fields.contents += new FlowPanel {
      contents += new Label("Value " + n + ":")
      contents += col
    }
    val content: Signal[String] = Signal { col.text() }
    fields() = content :: fields()
    frame.pack
  }

}
