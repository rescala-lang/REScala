package ex201x.swing.clickcounter

import scala.swing.*
import scala.swing.event.*

object ObserverSwingApp extends SimpleSwingApplication {
  def top =
    new MainFrame {

      /* Create the graphics */
      title = "Reactive Swing App"
      val button = new Button {
        text = "Click me"
      }
      val label = new Label {
        text = "No button clicks registered"
      }
      contents = new BoxPanel(Orientation.Vertical) {
        contents += button
        contents += label
        border = Swing.EmptyBorder(30, 30, 10, 30)
      }

      /* The logic */
      listenTo(button)
      var nClicks = 0
      reactions += {
        case ButtonClicked(b) =>
          nClicks += 1
          label.text = "Number of button clicks: " + nClicks
          if nClicks > 0 then
            button.text = "Click me again"
      }
    }
}
