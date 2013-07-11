package reader.gui

import java.awt.Dimension
import java.awt.Point
import java.awt.Toolkit

import react.events._
import scala.swing._

import javax.swing.ImageIcon
import reader.data.FeedStore
import reader.data.RSSChannel
import reader.data.RSSItem

/**
 * Responsible for displaying the content of the given FeedStore
 * The connections between the displayed content is mainly coordinated
 * by an initialized content mediator
 */
class GUI(store: FeedStore,
          notifications: Event[Any] = emptyevent,
          itemStatus: Event[Any] = emptyevent,
          fetcherState: Event[Any] = emptyevent)
            extends SimpleSwingApplication {
  val refreshButton = new EventButton("Refresh")
  val refresh = refreshButton.pressed.dropParam: Event[Unit] //#EVT
  
  val requestURLAddition = new ImperativeEvent[String]  //#EVT
  
  val refreshCheckbox = new EventCheckBox("auto refresh") { selected = true }
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
    
    val channels = store.channelsChanged
    val channelList = new EventListView[RSSChannel](channels) {
      renderer = ListView.Renderer(_.title)
      listData = store.channels.sorted
      peer.setVisibleRowCount(3)
    }
    
    val itemList = new EventListView[RSSItem](new ImperativeEvent[Iterable[RSSItem]]) { //#EVT
      renderer = ListView.Renderer(_.title)
    }
    
    val renderArea = new RssItemRenderPane
    
    val statusBar = new EventText(notifications)
    statusBar.preferredSize = new Dimension(framewidth / 3, 15)
    statusBar.horizontalAlignment = Alignment.Left
    
    val itemCountStatus = new EventText(itemStatus)
    itemCountStatus.preferredSize = new Dimension(framewidth / 3, 15)
    itemCountStatus.horizontalAlignment = Alignment.Right
    
    val fetcherStatus = new EventText(fetcherState)
    fetcherStatus.preferredSize = new Dimension(framewidth / 3, 15)
    fetcherStatus.horizontalAlignment = Alignment.Right
    
    val mediator = SyncAll.mediate(channelList, itemList, renderArea, store)
    
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
