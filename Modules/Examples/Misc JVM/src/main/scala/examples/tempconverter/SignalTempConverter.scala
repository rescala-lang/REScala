package examples.tempconverter

// EScala lib + behaviour extensions
import reactives.default.*

// Scala swing events
import scala.swing.*
import scala.swing.event.*

object SignalTempConverter extends SimpleSwingApplication {

  def top =
    new MainFrame {
      title = "Celsius/Fahrenheit Converter"
      object celsius    extends ReactiveTextfield { columns = 5 }
      object fahrenheit extends ReactiveTextfield { columns = 5 }

      // two global variables, holding the state
      val degree_f = Var(0)
      val degree_c = Var(0)

      // content of the textfields is well-defined
      fahrenheit.text = Signal { (degree_c.value * 9 / 5 + 32).toString }
      celsius.text = Signal { ((degree_f.value - 32) * 5 / 9).toString }

      // listener only changes model, doesn't care about the 'view' (setting the text of the textfields)
      listenTo(celsius, fahrenheit)
      reactions += {
        case EditDone(`fahrenheit`) => degree_f `set` fahrenheit.text.toInt
        case EditDone(`celsius`)    => degree_c `set` celsius.text.toInt
      }

      contents = new FlowPanel {
        contents += celsius
        contents += new Label(" Celsius  =  ")
        contents += fahrenheit
        contents += new Label(" Fahrenheit")
        border = Swing.EmptyBorder(15, 10, 10, 10)
      }
    }
}
