import scala.swing.{BorderPanel, GridPanel, Label, ListView, MainFrame, SimpleSwingApplication}

class GUI extends SimpleSwingApplication{

   def top = new MainFrame {

     val (framewidth, frameheight) = (840, 480)

     title = "RSS Reader"

      minimumSize = new java.awt.Dimension(framewidth, frameheight)

      val screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
      location = new java.awt.Point( (screenSize.width - framewidth) / 2
                                   , (screenSize.height - frameheight) / 2
                                   )

     val channelList = List()

     contents = new BorderPanel {
      val topPane = new GridPanel(1, 0) {
        contents += new BorderPanel {
          add(new Label("Choose Channel: "), BorderPanel.Position.West)
          add(new ListView(channelList), BorderPanel.Position.Center)

        }
      }
     }
   }




}
