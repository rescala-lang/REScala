package reshapes

import java.net.BindException
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
import scala.swing.Swing
import scala.swing.TabbedPane
import scala.swing.event.SelectionChanged

import drawing.DrawingSpaceState
import javax.swing.JOptionPane
import reshapes.actions.LoadAction
import reshapes.actions.MergeAction
import reshapes.actions.QuitAction
import reshapes.actions.SaveAction
import reshapes.actions.UndoAction
import reshapes.drawing.Command
import reshapes.drawing.NetworkSpaceState
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

object ReShapes extends SimpleSwingApplication {
  private val panelDrawingSpaceStates = new HashMap[TabbedPane.Page, (DrawingSpaceState, NetworkSpaceState)]
  private var drawingSpaceStateObservers: List[DrawingSpaceState => Unit] = Nil
  
  def registerDrawingSpaceStateObserver(obs: DrawingSpaceState => Unit) =
    drawingSpaceStateObservers ::= obs
  
  def unregisterDrawingSpaceStateObserver(obs: DrawingSpaceState => Unit) =
    drawingSpaceStateObservers = drawingSpaceStateObservers filterNot (_ == obs)
  
  def drawingSpaceState =
    if (ui.tabbedPane.selection.index != -1
          && (panelDrawingSpaceStates contains ui.tabbedPane.selection.page))
        panelDrawingSpaceStates(ui.tabbedPane.selection.page)._1
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
    layout(new InfoPanel) = Position.South
    layout(new ShapeSelectionPanel) = Position.West
    layout(new TabbedPane {
      pages += new TabbedPane.Page("Shapes", new ShapePanel)
      pages += new TabbedPane.Page("Commands", new CommandPanel)
    }) = Position.East
  }
    
  val menu = new MenuBar {
    val merge = new Menu("Merge with...")
    
    contents += new Menu("File") {
      contents += new MenuItem(Action("New tab") { addTab(new DrawingSpaceState, null) })
      contents += new MenuItem(Action("New network tab") { addNetworkTab })
      contents += new MenuItem(Action("Remove selected tab") { removeCurrentTab })
      contents += new Separator
      contents += new MenuItem(new SaveAction)
      contents += new MenuItem(new LoadAction)
      contents += new Separator
      contents += new MenuItem(new QuitAction(ReShapes.this))
    }
    
    contents += new Menu("Edit") {
      contents += new MenuItem(new UndoAction) {
        enabled = false
        
        def updateCommands(commands: List[Command]) =
          enabled = commands.nonEmpty
        
        private var currentState: DrawingSpaceState = null
        registerDrawingSpaceStateObserver{ state =>
          if (currentState != null)
            currentState.unregisterCommandsObserver(updateCommands)
          currentState = state
          if (currentState != null)
            currentState.registerCommandsObserver(updateCommands)
          
          updateCommands(if (currentState != null) currentState.commands else List.empty)
        }
      }
    }
    
    contents += new Menu("Tools") {
      contents += merge
    }
    
    def updateMerge() {
      merge.contents.clear
      for (tab <- ui.tabbedPane.pages)
        if (tab.index != ui.tabbedPane.selection.index)
          merge.contents += new MenuItem(new MergeAction(tab.title, panelDrawingSpaceStates(tab)._1))
    }
  }
  
  val newTabDialog = new NewTabDialog
  
  val serverDialog = new ServerDialog
  
  listenTo(ui.tabbedPane.selection)
  
  reactions += {
    case SelectionChanged(ui.tabbedPane) =>
      for (obs <- drawingSpaceStateObservers)
        obs(drawingSpaceState)
      if (ui.tabbedPane.pages.size > 0)
        menu.updateMerge
  }
  
  def addTab(drawingSpaceState: DrawingSpaceState, networkSpaceState: NetworkSpaceState) =
    if (newTabDialog.showDialog(ui.locationOnScreen))
      addDrawingPanel(
          generateDrawingPanel(
              newTabDialog.showIntersections.selected,
              newTabDialog.showCoordinates.selected,
              newTabDialog.showNames.selected,
              drawingSpaceState),
          networkSpaceState)
  
  def generateDrawingPanel(showIntersections: Boolean, showCoordinates: Boolean, showName: Boolean, state: DrawingSpaceState): DrawingPanel =
    (showIntersections, showCoordinates, showName) match {
      case (true, false, false) => new DrawingPanel(state) with ShowIntersection
      case (false, true, false) => new DrawingPanel(state) with ShowCoordinateSystem
      case (true, true, false) => new DrawingPanel(state) with ShowIntersection with ShowCoordinateSystem
      case (false, false, true) => new DrawingPanel(state) with ShowNameLabels
      case (true, false, true) => new DrawingPanel(state) with ShowIntersection with ShowNameLabels
      case (true, true, true) => new DrawingPanel(state) with ShowIntersection with ShowCoordinateSystem with ShowNameLabels
      case _ => new DrawingPanel(state)
    }
  
  def addDrawingPanel(panel: DrawingPanel, networkSpaceState: NetworkSpaceState) {
    val page = new TabbedPane.Page("drawing#%d".format(ui.tabbedPane.pages.size + 1), panel)
    panelDrawingSpaceStates(page) = (panel.state, networkSpaceState)
    ui.tabbedPane.pages += page
    menu.updateMerge
  }
  
  def addNetworkTab() {
    if (serverDialog.showDialog(ui.locationOnScreen) && serverDialog.inputIsValid)
      try {
        val state = new DrawingSpaceState
        addTab(
            state,
            new NetworkSpaceState(
                state,
                Swing.onEDTWait,
                serverDialog.hostname,
                serverDialog.commandPort,
                serverDialog.exchangePort,
                serverDialog.listenerPort))
      }
      catch {
        case e: ConnectException =>
          JOptionPane.showMessageDialog(null, "Server not available", "ConnectException", JOptionPane.ERROR_MESSAGE)
        case e: BindException =>
          JOptionPane.showMessageDialog(null, "Port cannot be bound", "BindException", JOptionPane.ERROR_MESSAGE)
        case e: Exception =>
          e.printStackTrace
          JOptionPane.showMessageDialog(null, "Invalid input!")
          addNetworkTab
      }
  }
  
  def removeCurrentTab() {
    if (ui.tabbedPane.pages.size > 0) {
      val (_, networkSpaceState) = panelDrawingSpaceStates(ui.tabbedPane.selection.page)
      if (networkSpaceState != null)
        networkSpaceState.dispose
      panelDrawingSpaceStates remove ui.tabbedPane.selection.page
      ui.tabbedPane.pages remove ui.tabbedPane.selection.index
      menu.updateMerge
    }
  }
}