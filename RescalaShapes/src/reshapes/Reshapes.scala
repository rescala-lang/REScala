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
import reshapes.ui.panels._
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
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import java.awt.Toolkit
import reshapes.ui.dialogs.ServerDialog
import reshapes.ui.dialogs.NewTabDialog
import scala.events.behaviour.Signal
import reshapes.ui.dialogs.DialogResult

object Reshapes extends SimpleSwingApplication {

  val tabbedPane = new TabbedPane()
  val currentTabIndex = new Var(0)
  // as event/Var
  var CurrentEvents = new Var(new Events())
  val panelEvents = new HashMap[TabbedPane.Page, Events]()

  // Panels
  var infoPanel = new InfoPanel()
  var shapePanel = new ShapePanel()
  var strokeInputPanel = new StrokeInputPanel()
  var shapeSelectionPanel = new ShapeSelectionPanel()
  var commandPanel = new CommandPanel()

  val ui = new BorderPanel {
    add(infoPanel, BorderPanel.Position.South)

    val eastPane = new TabbedPane() {
      this.pages += new TabbedPane.Page("Shapes", shapePanel)
      pages += new TabbedPane.Page("Commands", commandPanel)
    }
    add(eastPane, BorderPanel.Position.East)
    add(strokeInputPanel, BorderPanel.Position.North)
    add(shapeSelectionPanel, BorderPanel.Position.West)
    add(tabbedPane, BorderPanel.Position.Center)

    listenTo(tabbedPane.selection)

    reactions += {
      case SelectionChanged(`tabbedPane`) => {
        if (tabbedPane.pages.size > 0) {
          val currentEvents = panelEvents(tabbedPane.selection.page)
          CurrentEvents() = currentEvents

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
    val mergeMenu = new Menu("Merge with...")

    Signal { undo.enabled = !CurrentEvents().Commands().isEmpty }

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
    }
    contents += new Menu("Tools") {
      contents += mergeMenu
    }

    def updateMerge() = {
      mergeMenu.contents.clear()
      val mergableTabs = tabbedPane.pages filter (tab => tab.index != tabbedPane.selection.index) // all tabs except currently selected
      mergableTabs map (tab => mergeMenu.contents += new MenuItem(new MergeAction(tab.title, panelEvents(tab)))) // insert tabs in submenu
    }
  }

  def top = new MainFrame {
    title = "ReShapes"
    //this.maximize()
    preferredSize = new Dimension(1000, 600)

    menuBar = menu
    contents = ui
  }

  def addTab(event: Events = new Events()) {
    val dialog = new NewTabDialog()
    dialog.location = ui.locationOnScreen
    dialog.showDialog()
    if (dialog.dialogResult == DialogResult.OK) addDrawingPanel(dialog.generateDrawingPanel(event))
  }

  def addDrawingPanel(panel: DrawingPanel) {
    val page = new TabbedPane.Page("drawing#%d".format(tabbedPane.pages.size + 1), panel)
    panelEvents(page) = panel.event
    tabbedPane.pages += page
    menu.updateMerge()
  }

  def addNetworkTab() {
    val dialog = new ServerDialog()
    dialog.location = ui.locationOnScreen
    dialog.showDialog()
    if (dialog.inputIsValid() && dialog.dialogResult == DialogResult.OK) {
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
      panelEvents.remove(tabbedPane.selection.page)
      tabbedPane.pages.remove(tabbedPane.selection.index)
      menu.updateMerge()
    }
  }
}