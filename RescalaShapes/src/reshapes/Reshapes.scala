package reshapes

import java.net.ConnectException

import scala.collection.mutable.HashMap
import scala.swing.Action
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position
import scala.swing.Dimension
import scala.swing.MainFrame
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Separator
import scala.swing.SimpleSwingApplication
import scala.swing.TabbedPane
import scala.swing.event.SelectionChanged

import drawing.DrawingSpaceState
import javax.swing.JOptionPane
import reshapes.actions.LoadAction
import reshapes.actions.MergeAction
import reshapes.actions.QuitAction
import reshapes.actions.SaveAction
import reshapes.actions.UndoAction
import reshapes.drawing.DrawingSpaceState
import reshapes.drawing.NetworkSpaceState
import reshapes.ui.dialogs.DialogResult
import reshapes.ui.dialogs.NewTabDialog
import reshapes.ui.dialogs.ServerDialog
import reshapes.ui.panels.CommandPanel
import reshapes.ui.panels.DrawingPanel
import reshapes.ui.panels.InfoPanel
import reshapes.ui.panels.ShapePanel
import reshapes.ui.panels.ShapeSelectionPanel
import reshapes.ui.panels.ShowCoordinateSystem
import reshapes.ui.panels.ShowIntersection
import reshapes.ui.panels.ShowNameLabels
import reshapes.ui.panels.StrokeInputPanel

import reshapes.versions.observer._

object Reshapes extends SimpleSwingApplication {
  private val panelEvents = new HashMap[TabbedPane.Page, DrawingSpaceState]
  private var currentEventsObservers: List[DrawingSpaceState => Unit] = Nil
  
  def registerCurrentEventsObserver(obs: DrawingSpaceState => Unit) =
    currentEventsObservers ::= obs
  
  def unregisterCurrentEventsObserver(obs: DrawingSpaceState => Unit) =
    currentEventsObservers = currentEventsObservers filterNot (_ == obs)
  
  def currentEvents: DrawingSpaceState =
    if (ui.tabbedPane.selection.index != -1)
      panelEvents(ui.tabbedPane.selection.page)
    else
      null
  
  def top = new MainFrame {
    title = "ReShapes"
    preferredSize = new Dimension(1000, 600)
    menuBar = menu
    contents = ui
  }
  
  val ui = new BorderPanel {
    val tabbedPane = new TabbedPane
    layout(tabbedPane) = Position.Center
    layout(new StrokeInputPanel) = Position.North
    layout(new InfoPanel with InfoPanelInteraction) = Position.South
    layout(new ShapeSelectionPanel) = Position.West
    layout(new TabbedPane() {
      pages += new TabbedPane.Page("Shapes", new ShapePanel with ShapePanelInteraction)
      pages += new TabbedPane.Page("Commands", new CommandPanel with CommandPanelInteraction)
    }) = Position.East
  }
    
  val menu = new MenuBar {
    val mergeMenu = new Menu("Merge with...")
    
    contents += new Menu("File") {
      contents += new MenuItem(Action("New tab") { addTab() })
      contents += new MenuItem(Action("New network tab") { addNetworkTab() })
      contents += new MenuItem(Action("Remove selected tab") { removeCurrentTab() })
      contents += new Separator
      contents += new MenuItem(new SaveAction())
      contents += new MenuItem(new LoadAction())
      contents += new Separator
      contents += new MenuItem(new QuitAction())
    }
    
    contents += new Menu("Edit") {
      contents += new MenuItem(new UndoAction()) { enabled = false }
    }
    
    contents += new Menu("Tools") {
      contents += mergeMenu
    }
    
    def updateMerge() = {
      mergeMenu.contents.clear()
      val mergableTabs = ui.tabbedPane.pages filter (tab => tab.index != ui.tabbedPane.selection.index) // all tabs except currently selected
      mergableTabs map (tab => mergeMenu.contents += new MenuItem(new MergeAction(tab.title, panelEvents(tab)))) // insert tabs in submenu
    }
  }
  
  listenTo(ui.tabbedPane.selection)
  
  reactions += {
    case SelectionChanged(ui.tabbedPane) =>
      for (obs <- currentEventsObservers)
        obs(currentEvents)
      if (ui.tabbedPane.pages.size > 0)
        menu.updateMerge()
  }
  
  def addTab(event: DrawingSpaceState = new DrawingSpaceState) {
    val dialog = new NewTabDialog()
    dialog.location = ui.locationOnScreen
    dialog.showDialog()
    if (dialog.dialogResult == DialogResult.OK) {
      addDrawingPanel(generateDrawingPanel(dialog.showIntersections.selected,
        dialog.showCoordinates.selected, dialog.showNames.selected, event))
    }
  }
  
  def generateDrawingPanel(showIntersections: Boolean, showCoordinates: Boolean, showName: Boolean, state: DrawingSpaceState): DrawingPanel = {
    (showIntersections, showCoordinates, showName) match {
      case (true, false, false) => return new DrawingPanel(state) with ShowIntersection with DrawingPanelInteraction
      case (false, true, false) => return new DrawingPanel(state) with ShowCoordinateSystem with DrawingPanelInteraction
      case (true, true, false) => return new DrawingPanel(state) with ShowIntersection with ShowCoordinateSystem with DrawingPanelInteraction
      case (false, false, true) => return new DrawingPanel(state) with ShowNameLabels with DrawingPanelInteraction
      case (true, false, true) => return new DrawingPanel(state) with ShowIntersection with ShowNameLabels with DrawingPanelInteraction
      case (true, true, true) => return new DrawingPanel(state) with ShowIntersection with ShowCoordinateSystem with ShowNameLabels with DrawingPanelInteraction
      case _ => return new DrawingPanel(state) with DrawingPanelInteraction
    }
  }
  
  def addDrawingPanel(panel: DrawingPanel) {
    val page = new TabbedPane.Page("drawing#%d".format(ui.tabbedPane.pages.size + 1), panel)
    panelEvents(page) = panel.event
    ui.tabbedPane.pages += page
    menu.updateMerge()
  }
  
  
  def addNetworkTab() {
    val dialog = new ServerDialog()
    dialog.location = ui.locationOnScreen
    dialog.showDialog()
    if (dialog.inputIsValid() && dialog.dialogResult == DialogResult.OK) {
      try {
        addTab((new NetworkSpaceState(dialog.hostname, dialog.commandPort, dialog.exchangePort, dialog.listenerPort) with NetworkSpaceStateInteraction))
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
  
  
  def removeCurrentTab() {
    if (ui.tabbedPane.pages.size > 0) {
      panelEvents.remove(ui.tabbedPane.selection.page)
      ui.tabbedPane.pages.remove(ui.tabbedPane.selection.index)
      menu.updateMerge()
    }
  }
}