package reswing.reader.gui

import reactives.default.*
import reswing.*
import reswing.reader.data.{FeedStore, RSSChannel, RSSItem}

import java.awt.{Dimension, Point, Toolkit}
import javax.swing.ImageIcon
import scala.swing.*

/** Responsible for displaying the content of the given FeedStore
  * The connections between the displayed content is mainly coordinated
  * by an initialized content mediator
  */
class GUI(
    store: FeedStore,
    notifications: Signal[String] = Signal { "" },
    itemStatus: Signal[String] = Signal { "" },
    fetcherState: Signal[String] = Signal { "" }
) extends SimpleSwingApplication {
  val refreshButton = new ReButton("Refresh")
  val refresh       = refreshButton.clicked

  val requestURLAddition = Evt[String]() // #EVT

  val refreshCheckbox = new ReCheckBox("auto refresh", selected = true)
  def refreshAllowed  = refreshCheckbox.selected

  def top =
    new MainFrame {
      val quitAction = swing.Action("Quit") { quit() }
      val urlDialogAction = swing.Action("Add url") {
        val input = Dialog.showInput(
          null,
          "Please enter a feed url:",
          "Add URL",
          Dialog.Message.Question,
          Swing.EmptyIcon,
          Nil,
          ""
        )
        input.foreach { requestURLAddition.fire(_) }
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

      val channelList = new ReListViewEx[RSSChannel](
        Signal { store.channels.value.keys.toSeq }, // #SIG
        visibleRowCount = 3
      ) {
        peer.renderer = ListView.Renderer(_.title)
      }

      val selectedChannelItems = Signal.dynamic { // #SIG
        channelList.selectedItem.value match {
          case Some(channel) => store.channels.value.get(channel) match {
              case Some(items) => items.value.toSeq
              case _           => Seq.empty
            }
          case _ => Seq.empty
        }
      }

      val itemList = new ReListViewEx[RSSItem](selectedChannelItems) {
        peer.renderer = ListView.Renderer(_.title)
      }

      val renderArea = new RssItemRenderPane(itemList.selectedItem)

      val statusBar = new ReLabel(
        notifications,
        preferredSize = ReSwingValue(new Dimension(framewidth / 3, 15)),
        horizontalAlignment = Alignment.Left
      )

      val itemCountStatus = new ReLabel(
        itemStatus,
        preferredSize = ReSwingValue(new Dimension(framewidth / 3, 15)),
        horizontalAlignment = Alignment.Left
      )

      val fetcherStatus = new ReLabel(
        fetcherState,
        preferredSize = ReSwingValue(new Dimension(framewidth / 3, 15)),
        horizontalAlignment = Alignment.Left
      )

      contents = new BorderPanel {
        val topPane = new GridPanel(1, 1) {
          contents += new BorderPanel {
            add(new Label("Choose Channel: "), BorderPanel.Position.West)
            add(new ScrollPane(channelList), BorderPanel.Position.Center)
            add(
              new GridPanel(2, 1) {
                contents += refreshButton
                contents += refreshCheckbox
              },
              BorderPanel.Position.East
            )
          }
        }

        val splitPane = new SplitPane(Orientation.Vertical, new ScrollPane(itemList), new ScrollPane(renderArea))

        val mainPane = new SplitPane(Orientation.Horizontal, topPane, splitPane)

        add(mainPane, BorderPanel.Position.Center)
        add(
          new GridPanel(1, 3) {
            contents += statusBar
            contents += itemCountStatus
            contents += fetcherStatus
          },
          BorderPanel.Position.South
        )
      }

      private def configure(): Unit = {
        title = "RSS Reader"
        iconImage = new ImageIcon("res/icon.png").getImage

        minimumSize = new Dimension(framewidth, frameheight)

        val screenSize = Toolkit.getDefaultToolkit.getScreenSize
        location = new Point((screenSize.width - framewidth) / 2, (screenSize.height - frameheight) / 2)
      }
    }
}
