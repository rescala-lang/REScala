import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import scala.swing.BorderPanel
import scala.swing.GridPanel
import scala.swing.Label
import scala.swing.ScrollPane
import scala.swing.ListView

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