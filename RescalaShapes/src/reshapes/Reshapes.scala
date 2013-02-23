package reshapes

import scala.collection.mutable.HashMap
import scala.events.behaviour.Var
import scala.swing.event.SelectionChanged
import scala.swing.Dimension
import scala.swing.Action
import scala.swing.BorderPanel
import scala.swing.Frame
import scala.swing.MainFrame
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Separator
import scala.swing.SimpleSwingApplication
import scala.swing.TabbedPane
import reshapes.actions.LoadAction
import reshapes.actions.QuitAction
import reshapes.actions.SaveAction
import reshapes.actions.UndoAction
import reshapes.panels.CommandPanel
import reshapes.panels.DrawingPanel
import reshapes.panels.InfoPanel
import reshapes.panels.ShapePanel
import reshapes.panels.ShapeSelectionPanel
import reshapes.panels.StrokeInputPanel
import reshapes.Events
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import org.omg.CORBA.Environment
import java.awt.Point
import reshapes.actions.MergeAction
import scala.swing.Dialog
import scala.swing.Label
import scala.swing.Button
import javax.swing.JOptionPane
import java.net.ConnectException

object Reshapes extends SimpleSwingApplication {

  val tabbedPane = new TabbedPane()
  val currentTabIndex = new Var(0)
  // as event/Var
  var CurrentEvents: Events = new Events()
  val panelEvents = new HashMap[Int, Events]()

  // Panels
  var infoPanel = new InfoPanel(CurrentEvents)
  var shapePanel = new ShapePanel(CurrentEvents)
  var strokeInputPanel = new StrokeInputPanel(CurrentEvents)
  var shapeSelectionPanel = new ShapeSelectionPanel(CurrentEvents)
  var commandPanel = new CommandPanel(CurrentEvents)

  val ui = new BorderPanel {
    add(infoPanel, BorderPanel.Position.South)
    add(shapePanel, BorderPanel.Position.East)
    add(strokeInputPanel, BorderPanel.Position.North)
    add(shapeSelectionPanel, BorderPanel.Position.West)
    add(tabbedPane, BorderPanel.Position.Center)

    listenTo(tabbedPane.selection)

    reactions += {
      case SelectionChanged(`tabbedPane`) => {
        if (tabbedPane.pages.size > 0) {
          val currentEvents = panelEvents(tabbedPane.selection.index)
          infoPanel.events = currentEvents
          shapePanel.events = currentEvents
          strokeInputPanel.events = currentEvents
          shapeSelectionPanel.events = currentEvents
          commandPanel.events = currentEvents

          CurrentEvents = currentEvents

          menu.updateMerge()
        }
      }
    }
  }

  val menu = new MenuBar {
    val newTab = new MenuItem(Action("New tab") { addTab() })
    val newNetworkTab = new MenuItem(Action("New network tab") { addNetworkTab() })
    val closeTab = new MenuItem(Action("Remove selected tab") { removeTab() })
    val save = new MenuItem(new SaveAction())
    val load = new MenuItem(new LoadAction())
    val quit = new MenuItem(new QuitAction())
    val undo = new MenuItem(new UndoAction()) { enabled = false }
    val cmdWindow = new MenuItem(Action("show command window") { commandWindow.visible = true })
    val mergeMenu = new Menu("Merge with...")

    CurrentEvents.Commands.changed += (commands => undo.enabled = !commands.isEmpty)

    contents += new Menu("File") {
      contents += newTab
      contents += newNetworkTab
      contents += closeTab
      contents += new Separator
      contents += save
      contents += load
      contents += new Separator
      contents += quit
    }
    contents += new Menu("Edit") {
      contents += undo
      contents += new Separator
      contents += cmdWindow
    }
    contents += new Menu("Tools") {
      contents += mergeMenu
    }

    def updateMerge() = {
      mergeMenu.contents.clear()
      val mergableTabs = tabbedPane.pages filter (tab => tab.index != tabbedPane.selection.index) // all tabs except currently selected
      mergableTabs map (tab => mergeMenu.contents += new MenuItem(new MergeAction(tab.title, panelEvents(tab.index)))) // insert tabs in submenu
    }
  }

  def top = new MainFrame {
    title = "ReShapes"
    preferredSize = new Dimension(1000, 500)
    this.location = new Point(commandWindow.location.x + commandWindow.size.width, commandWindow.location.y)

    menuBar = menu
    contents = ui

    commandWindow.visible = true
  }

  def commandWindow = new Frame {
    title = "Command list"
    preferredSize = new Dimension(300, 500)
    contents = commandPanel
  }

  def addTab(event: Events = new Events()) {
    panelEvents(tabbedPane.pages.size) = event
    val panel = new DrawingPanel(event)
    tabbedPane.pages += new TabbedPane.Page("drawing#%d".format(tabbedPane.pages.size + 1), panel)
    menu.updateMerge()
  }

  def addNetworkTab() {
    val dialog = new ServerDialog()
    dialog.showDialog()
    if (dialog.inputIsValid()) {
      try {
        addTab(new NetworkEvents(dialog.hostname, dialog.commandPort, dialog.exchangePort, dialog.listenerPort))
      } catch {
        case e: ConnectException =>
          JOptionPane.showMessageDialog(null, "Server not available", "ConnectException", JOptionPane.ERROR_MESSAGE)
        case e: Exception =>
          e.printStackTrace()
          JOptionPane.showMessageDialog(null, "Invalid input!")
          addNetworkTab()
      }
    }
  }

  /**
   * Removes the currently selected tab and its associated Event.
   */
  def removeTab() {
    if (tabbedPane.pages.size > 0) {
      panelEvents.remove(tabbedPane.selection.index)
      tabbedPane.pages.remove(tabbedPane.selection.index)
      menu.updateMerge()
    }
  }

  addTab() // add one tab at startup
}