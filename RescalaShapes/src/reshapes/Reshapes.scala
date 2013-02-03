package reshapes

import java.awt.Color
import scala.Array.canBuildFrom
import scala.swing.event.ButtonClicked
import scala.swing.event.EditDone
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

object Reshapes extends SimpleSwingApplication {

  val tabbedPane = new TabbedPane()
  // as event/Var
  val drawingPanels = new Var(MutableList[(String, TabbedPane.Page)]())

  val ui = new BorderPanel {

    add(new InfoPanel(), BorderPanel.Position.South)
    add(new ShapePanel(), BorderPanel.Position.East)
    add(new StrokeInputPanel(), BorderPanel.Position.North)
    add(new ShapeSelectionPanel(), BorderPanel.Position.West)
    tabbedPane.pages += new TabbedPane.Page("newdrawing", new DrawingPanel())
    add(tabbedPane, BorderPanel.Position.Center)

    // reactions
    //listenTo(mouse.clicks)
  }

  val menu = new MenuBar {
    val newTab = new MenuItem(Action("New tab") { addTab() })
    val closeTab = new MenuItem(Action("Remove selected tab") { removeTab() })
    val save = new MenuItem(new SaveAction())
    val load = new MenuItem(new LoadAction())
    val quit = new MenuItem(new QuitAction())
    val undo = new MenuItem(new UndoAction()) { enabled = false }
    val cmdWindow = new MenuItem(Action("show command window") { commandWindow.visible = true })

    Events.Commands.changed += (commands => undo.enabled = !commands.isEmpty)

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
    contents = new CommandPanel()
  }

  def addTab() {
    throw new NotImplementedException()
  }

  def removeTab() {
    throw new NotImplementedException()
  }
}