package ex201x.swing.tempconverter

import scala.swing.*
import scala.swing.event.*

object TempConverterObserver extends SimpleSwingApplication {
  def top =
    new MainFrame {
      title = "Celsius/Fahrenheit Converter"
      object celsius    extends TextField(10) { columns = 5 }
      object fahrenheit extends TextField(20) { columns = 5 }

      listenTo(celsius, fahrenheit)
      reactions += {
        case EditDone(`fahrenheit`) =>
          val f = fahrenheit.text.toInt
          val c = (f - 32) * 5 / 9
          celsius.text = c.toString
        case EditDone(`celsius`) =>
          val c = celsius.text.toInt
          val f = c * 9 / 5 + 32
          fahrenheit.text = f.toString
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
