package reader.gui

import java.awt.Dimension
import java.awt.Point
import java.awt.Toolkit

import scala.swing._

import javax.swing.ImageIcon
import macro.SignalMacro.{SignalM => Signal}
import react.Signal
import react.SignalSynt
import react.StaticSignal
import react.events.Event
import react.events.ImperativeEvent
import reader.data.FeedStore
import reader.data.RSSChannel
import reader.data.RSSItem

/**
 * Responsible for displaying the content of the given FeedStore
 * The connections between the displayed content is mainly coordinated
 * by an initialized content mediator
 */
class GUI(store: FeedStore,
          notifications: Signal[Any] = StaticSignal(){ () },
          itemStatus: Signal[Any] = StaticSignal(){ () },
          fetcherState: Signal[Any] = StaticSignal(){ () })
            extends SimpleSwingApplication {
  val refreshButton = new ReButton("Refresh")
  val refresh: Event[Unit] = refreshButton.pressed.dropParam: Event[Unit] //#EF
  
  val requestURLAddition = new ImperativeEvent[String] //#EVT
  
  val refreshCheckbox = new ReCheckBox("auto refresh") { selected = true }
  def refreshAllowed = refreshCheckbox.selected
  
  def top = new MainFrame {
    val quitAction = swing.Action("Quit") { quit }
    val urlDialogAction = swing.Action("Add url") {
      val input = Dialog.showInput(null,
                                   "Please enter a feed url:",
                                   "Add URL",
                                   Dialog.Message.Question,
                                   Swing.EmptyIcon,
                                   Nil,
                                   "")
      input.foreach { requestURLAddition(_) }
    }
    
    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(quitAction)
      }
      contents += new Menu("Edit") {
        contents += new MenuItem(urlDialogAction)
      }
    }
    
    val (framewidth, frameheight) = (840, 480)
    configure
    
    val channelList = new ReListView[RSSChannel](Signal { store.channels().keys.toIterable }) { //#SIG
      renderer = ListView.Renderer(_.title)
      peer.setVisibleRowCount(3)
    }
    
    val selectedChannelItems = Signal { //#SIG
      channelList.selectedItem() match {
        case Some(channel) => store.channels().get(channel) match {
          case Some(items) => items().toIterable
          case _ => Iterable.empty
        }
        case _ => Iterable.empty
      }
    }
    
    val itemList = new ReListView[RSSItem](selectedChannelItems) {
      renderer = ListView.Renderer(_.title)
    }
    
    val renderArea = new RssItemRenderPane(itemList.selectedItem)
    
    val statusBar = new ReText(notifications)
    statusBar.preferredSize = new Dimension(framewidth / 3, 15)
    statusBar.horizontalAlignment = Alignment.Left
    
    val itemCountStatus = new ReText(itemStatus)
    itemCountStatus.preferredSize = new Dimension(framewidth / 3, 15)
    itemCountStatus.horizontalAlignment = Alignment.Right
    
    val fetcherStatus = new ReText(fetcherState)
    fetcherStatus.preferredSize = new Dimension(framewidth / 3, 15)
    fetcherStatus.horizontalAlignment = Alignment.Right
    
    contents = new BorderPanel {
      val topPane = new GridPanel(1, 1) {
        contents += new BorderPanel {
          add(new Label("Choose Channel: "), BorderPanel.Position.West)
          add(new ScrollPane(channelList), BorderPanel.Position.Center)
          add(new GridPanel(2, 1) {
            contents += refreshButton
            contents += refreshCheckbox
          }, BorderPanel.Position.East)
        }
      }
      
      val splitPane = new SplitPane(Orientation.Vertical,
                                    new ScrollPane(itemList),
                                    new ScrollPane(renderArea))
      
      val mainPane = new SplitPane(Orientation.Horizontal,
                                   topPane,
                                   splitPane)
      
      add(mainPane, BorderPanel.Position.Center)
      add(new GridPanel(1, 3) {
        contents += statusBar
        contents += itemCountStatus
        contents += fetcherStatus
      }, BorderPanel.Position.South)
    }
    
    private def configure() {
      title = "RSS Reader"
      iconImage = new ImageIcon("res/icon.png").getImage
      
      minimumSize = new Dimension(framewidth, frameheight)
      
      val screenSize = Toolkit.getDefaultToolkit.getScreenSize
      location = new Point((screenSize.width - framewidth) / 2,
                           (screenSize.height - frameheight) / 2)
    }
  }
}
