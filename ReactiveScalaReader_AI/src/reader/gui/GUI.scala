package reader.gui

import java.awt.Dimension
import java.awt.Point
import java.awt.Toolkit

import scala.annotation.migration
import scala.events.ImperativeEvent
import scala.events.behaviour.Signal
import scala.swing._

import javax.swing.ImageIcon
import reader.data.FeedStore
import reader.data.RSSChannel
import reader.data.RSSItem
import reader.gui.ReactiveSwingConversions.eventButtonToEvent

/**
 * Responsible for displaying the content of the given FeedStore
 * The connections between the displayed content is mainly coordinated
 * by an initialized content mediator
 */
class GUI(val store: FeedStore,
          val itemStatus: Signal[Any] = new Signal)
            extends SimpleSwingApplication {
  val requestURLAddition = new ImperativeEvent[String]
  
  val notifications = new ImperativeEvent[Any]
  val refresh = new ImperativeEvent[Unit]
  val menuExit = new ImperativeEvent[Unit]
  val frameExit = new ImperativeEvent[Unit]
  
  (menuExit || frameExit) += { _ => quit }
  
  val refreshButton = new ReButton("Refresh")
  refreshButton += { _ => refresh() }
  
  val refreshCheckbox = new ReCheckBox("auto refresh") { selected = true }
  
  def refreshAllowed = refreshCheckbox.selected
  
  def top = new MainFrame {
    val quitAction = swing.Action("Quit") { menuExit() }
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
    
    val channelList = new ReListView[RSSChannel](Signal { store.channels().keys.toIterable }) {
      renderer = ListView.Renderer(_.title)
      peer.setVisibleRowCount(3)
    }
    
    val selectedChannelItems = Signal { channelList.selectedItem() match {
      case Some(channel) => store.channels().get(channel) match {
        case Some(item) => item().toIterable
        case _ => Iterable.empty
      }
      case _ => Iterable.empty
    } }
    
    val itemList = new ReListView[RSSItem](selectedChannelItems) {
      renderer = ListView.Renderer(_.title)
    }
    
    val renderArea = new RssItemRenderPane(itemList.selectedItem)
    
    val statusBar = new ReText(notifications latest "")
    statusBar.preferredSize = new Dimension(framewidth / 2, 15)
    statusBar.horizontalAlignment = Alignment.Left
    
    val itemCountStatus = new ReText(itemStatus)
    itemCountStatus.preferredSize = new Dimension(framewidth / 2, 15)
    itemCountStatus.horizontalAlignment = Alignment.Right
    
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
      add(new GridPanel(1, 2) {
        contents += statusBar
        contents += itemCountStatus
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
