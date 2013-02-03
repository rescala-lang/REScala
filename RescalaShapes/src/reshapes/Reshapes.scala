package reshapes

import java.awt.Color
import scala.Array.canBuildFrom
import scala.swing.event._
import scala.swing.Dimension
import scala.swing.Action
import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.FlowPanel
import scala.swing.Frame
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Orientation
import scala.swing.Separator
import scala.swing.SimpleSwingApplication
import scala.swing.TextField
import reshapes.actions.LoadAction
import reshapes.actions.QuitAction
import reshapes.actions.SaveAction
import reshapes.actions.UndoAction
import reshapes.figures.Line
import reshapes.figures.Oval
import reshapes.panels.CommandPanel
import reshapes.panels.DrawingPanel
import reshapes.panels.InfoPanel
import reshapes.panels.ShapePanel
import reshapes.panels.ShapeSelectionPanel
import reshapes.panels.StrokeInputPanel
import scala.swing.TabbedPane
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import scala.collection.mutable.MutableList
import scala.events.behaviour.Var
import scala.swing.event.SelectionChanged
import scala.events.scalareact
import scala.collection.mutable.HashMap

object Reshapes extends SimpleSwingApplication {

  val tabbedPane = new TabbedPane()
  val currentTabIndex = new Var(0)
  // as event/Var
  var CurrentEvents: Events = new Events()
  val drawingPanels = new HashMap[Int, Events]()
  drawingPanels(0) = CurrentEvents

  // Panels
  val infoPanel = new InfoPanel(CurrentEvents)
  val shapePanel = new ShapePanel(CurrentEvents)
  val strokeInputPanel = new StrokeInputPanel(CurrentEvents)
  val shapeSelectionPanel = new ShapeSelectionPanel(CurrentEvents)
  val drawingPanel = new DrawingPanel(CurrentEvents)

  val ui = new BorderPanel {
    add(infoPanel, BorderPanel.Position.South)
    add(shapePanel, BorderPanel.Position.East)
    add(strokeInputPanel, BorderPanel.Position.North)
    add(shapeSelectionPanel, BorderPanel.Position.West)
    tabbedPane.pages += new TabbedPane.Page("newdrawing", drawingPanel)
    add(tabbedPane, BorderPanel.Position.Center)

    listenTo(tabbedPane.selection)

    reactions += {
      case SelectionChanged(`tabbedPane`) => {
        val currentEvents = drawingPanels(tabbedPane.selection.index)
        infoPanel.events = currentEvents
        shapePanel.events = currentEvents
        strokeInputPanel.events = currentEvents
        shapeSelectionPanel.events = currentEvents
        drawingPanel.events = currentEvents
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
    title = "ReShapes";
    preferredSize = new Dimension(1000, 500)

    menuBar = menu
    contents = ui

    commandWindow.visible = true
  }

  def commandWindow = new Frame {
    title = "Command list"
    preferredSize = new Dimension(300, 500)
    contents = new CommandPanel(CurrentEvents)
  }

  def addTab() {
    val event = new Events()
    tabbedPane.pages += new TabbedPane.Page("newdrawing", new DrawingPanel(event))
    drawingPanels(tabbedPane.pages.size - 1) = event
  }

  def removeTab() {
    throw new NotImplementedException()
  }
}