package ex201x.swing.tempconverter

// Escala lib + behaviour extensions
import reactives.default.*

// Scala swing events
import scala.swing.*

object ReactiveTempConverter extends SimpleSwingApplication {

  def top =
    new MainFrame {
      title = "Celsius/Fahrenheit Converter"
      object celsius    extends ReactiveTextfield { columns = 5 }
      object fahrenheit extends ReactiveTextfield { columns = 5 }

      fahrenheit.text = Signal { "" + (("0" + celsius.text_out.value).toInt * 9 / 5 + 32) }
      celsius.text = Signal { "" + (("0" + fahrenheit.text_out.value).toInt - 32) * 5 / 9 }

      contents = new FlowPanel {
        contents += celsius
        contents += new Label(" Celsius  =  ")
        contents += fahrenheit
        contents += new Label(" Fahrenheit")
        border = Swing.EmptyBorder(15, 10, 10, 10)
      }
    }
}
