package reader.gui

import reader.common.implicits._
import reader.data._
import reader.gui.ReactiveSwingConversions._

import swing._
import actors._
import actors.Actor._
import javax.swing._
import scala.collection.mutable.ListBuffer
import scala.swing.event._
import java.net.URL
import scala.events.ImperativeEvent

import scala.events.behaviour._

/**
 * Responsible for displaying the content of the given FeedStore
 * The connections between the displayed content is mainly coordinated
 * by an initialized content mediator
 */

trait ReactiveText {
  def text_=(s : String)
  def text_=(value: Signal[String]) {    
    this.text_=(value.getValue)
    value.changed += {(t : String) => this.text_=(t)}
  }
}
class ReactiveLabel extends Label with ReactiveText
class ReactiveButton extends Button with ReactiveText {
	// wrap the event to escala
	val clicked = new ImperativeEvent[ButtonClicked]
	reactions += { case c @ ButtonClicked(_) => clicked(c) }
}

class GUI(val store: FeedStore) extends SimpleSwingApplication {

  val requestURLAddition = new ImperativeEvent[String]

  val notifications = new ImperativeEvent[Any]
  val itemStatus = new ImperativeEvent[Any]
  val refresh = new ImperativeEvent[Unit]
  val menuExit = new ImperativeEvent[Unit]
  val frameExit = new ImperativeEvent[Unit]

  (menuExit || frameExit) += { _ => shutdown(); sys.exit(0) }

//  val refreshButton = new EventButton("Refresh")
//  refreshButton += { _ => refresh() }
  
  val button = new ReactiveButton
  val label = new ReactiveLabel
  
  val refreshActive : Signal[Boolean] = Signal{
    //shows actual state of refreshing
  }
  
  label.text = Signal { if(refreshActive) "Refreshing..." else "Refresh" }

  val refreshCheckbox = new EventCheckBox("auto refresh") { selected = true }

  def refreshAllowed = refreshCheckbox.selected

  def top = new MainFrame {
    val quitAction = swing.Action("Quit") { menuExit() }
    val urlDialogAction = swing.Action("Add url") {
      val input = Dialog.showInput( null
                                  , "Please enter a feed url:"
                                  , "Add URL"
                                  , Dialog.Message.Question
                                  , Swing.EmptyIcon
                                  , Nil
                                  , "")
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
    configure()

    val channels = store.channelsChanged
//    val channelList = new EventListView[RSSChannel](channels) {
//      renderer = ListView.Renderer(_.title)
//      listData = store.channels.sorted
//      peer.setVisibleRowCount(3)
//    }
    
    val channelList = Signal{
      // Signal with channels
//    	renderer = ListView.Renderer(_.title)
//    	listData = store.channels.sorted
//    	peer.setVisibleRowCount(3)
    }

    val itemList = new EventListView[RSSItem](new ImperativeEvent[Iterable[RSSItem]]) {
      renderer = ListView.Renderer(_.title)
    }

    val renderArea = new RssItemRenderPane

    val statusBar = new EventText(notifications)
    statusBar.preferredSize = new Dimension(framewidth / 2, 15)
    statusBar.horizontalAlignment = Alignment.Left

    val itemCountStatus = new EventText(itemStatus)
    itemCountStatus.preferredSize = new Dimension(framewidth / 2, 15)
    itemCountStatus.horizontalAlignment = Alignment.Right

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

      val splitPane = new SplitPane( Orientation.Vertical
                                   , new ScrollPane(itemList)
                                   , new ScrollPane(renderArea)
                                   )

      val mainPane = new SplitPane( Orientation.Horizontal
                                      , topPane
                                      , splitPane)

      add(mainPane, BorderPanel.Position.Center)
      add(new GridPanel(1, 2) {
        contents += statusBar
        contents += itemCountStatus
      }, BorderPanel.Position.South)
    }

    private def configure() {
      title = "RSS Reader"
      iconImage = new ImageIcon("res/icon.png").getImage

      minimumSize = new java.awt.Dimension(framewidth, frameheight)

      val screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
      location = new java.awt.Point( (screenSize.width - framewidth) / 2
                                   , (screenSize.height - frameheight) / 2
                                   )
    }
  }

  override def quit() { frameExit() }
}

