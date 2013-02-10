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
        val currentEvents = panelEvents(tabbedPane.selection.index)
        infoPanel.events = currentEvents
        shapePanel.events = currentEvents
        strokeInputPanel.events = currentEvents
        shapeSelectionPanel.events = currentEvents
        commandPanel.events = currentEvents

        CurrentEvents = currentEvents
      }
    }
  }

  val menu = new MenuBar {
    val newTab = new MenuItem(Action("New tab") { addTab() })
    val closeTab = new MenuItem(Action("Remove selected tab") { removeTab() })
    val save = new MenuItem(new SaveAction())
    val load = new MenuItem(new LoadAction())
    val quit = new MenuItem(new QuitAction())
    val undo = new MenuItem(new UndoAction()) { enabled = false }
    val cmdWindow = new MenuItem(Action("show command window") { commandWindow.visible = true })

    CurrentEvents.Commands.changed += (commands => undo.enabled = !commands.isEmpty)

    contents += new Menu("File") {
      contents += newTab
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
  }

  def top = new MainFrame {
    title = "ReShapes"
    preferredSize = new Dimension(1000, 500)

    menuBar = menu
    contents = ui

    commandWindow.visible = true
  }

  def commandWindow = new Frame {
    title = "Command list"
    preferredSize = new Dimension(300, 500)
    contents = commandPanel
  }

  def addTab() {
    val event = new Events()
    panelEvents(tabbedPane.pages.size) = event
    val panel = new DrawingPanel(event)
    tabbedPane.pages += new TabbedPane.Page("newdrawing", panel)
  }

  def removeTab() {
    throw new NotImplementedException()
  }
}