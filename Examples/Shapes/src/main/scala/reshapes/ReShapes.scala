package reshapes

import java.net.BindException
import java.net.ConnectException

import scala.collection.mutable.HashMap
import scala.swing.Action
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position
import scala.swing.Component
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
import makro.SignalMacro.{SignalM => Signal}
import rescala.Signal
import rescala.Var
import rescala.events.Event
import rescala.events.ImperativeEvent
import reshapes.actions.LoadAction
import reshapes.actions.SaveAction
import reshapes.drawing.Command
import reshapes.drawing.MergeDrawingSpaces
import reshapes.drawing.NetworkSpaceState
import reshapes.figures.Shape
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
import reshapes.util.ReactiveUtil.UnionEvent
import reshapes.util.ReactiveUtil.bilateralValues
import reswing.ReSwingValue
import reswing.ReMenu
import reswing.ReMenuItem
import reswing.ReMenuItem.toMenuItem

object ReShapes extends SimpleSwingApplication {
  private val panelDrawingSpaceStates = new HashMap[TabbedPane.Page, (DrawingSpaceState, NetworkSpaceState)]
  
  val drawingSpaceState = Var[DrawingSpaceState](null) //#VAR
  
  def top = new MainFrame {
    title = "ReShapes"
    preferredSize = new Dimension(1000, 600)
    menuBar = menu
    contents = ui
  }
  
  val ui = new BorderPanel {
    val tabbedPane = new TabbedPane
    val strokeInputPanel = new StrokeInputPanel
    val shapeSelectionPanel = new ShapeSelectionPanel
    val shapePanel = new ShapePanel
    val commandPanel = new CommandPanel
    
    layout(tabbedPane) = Position.Center
    layout(strokeInputPanel) = Position.North
    layout(new InfoPanel) = Position.South
    layout(shapeSelectionPanel) = Position.West
    layout(new TabbedPane {
      pages += new TabbedPane.Page("Shapes", shapePanel)
      pages += new TabbedPane.Page("Commands", commandPanel)
    }) = Position.East
  }
    
  val menu = new MenuBar {
    val undo = new ReMenuItem("Undo", enabled = Signal {  //#SIG //#IS( // )
      drawingSpaceState() != null && drawingSpaceState().commands().nonEmpty })
    
    val merge = new ReMenu(
      text = "Merge with...", //#SIG //#IS( // )
      contents = Signal { //#SIG //#IS( // )
        itemsEvents() map { case (btn, _) => btn } }
    )
    
    final lazy val merged = UnionEvent(Signal { //#SIG //#UE( //#EVT //#IF )
        itemsEvents() map { case (_, ev) => ev } })
    
    lazy val update = new ImperativeEvent[Unit]   //#EVT
    
    private lazy val itemsEvents: Signal[Seq[(Component, Event[Command])]] =  //#SIG
      (update map { _: Any =>  //#EF
        (ui.tabbedPane.pages filter
          { tab => tab.index != ui.tabbedPane.selection.index } map
          { tab =>
            val item = new ReMenuItem(tab.title) //#IS( // )
            val command = item.clicked map { _: Any =>  //#EF
              new MergeDrawingSpaces(panelDrawingSpaceStates(tab)._1) }
            (item: Component, command)
          })
      }) latest Seq.empty  //#IF
    
    contents += new Menu("File") {
      contents += new MenuItem(Action("New tab") { addTab() })
      contents += new MenuItem(Action("New network tab") { addNetworkTab })
      contents += new MenuItem(Action("Remove selected tab") { removeCurrentTab })
      contents += new Separator
      contents += new MenuItem(new SaveAction)
      contents += new MenuItem(new LoadAction)
      contents += new Separator
      contents += new ReMenuItem(text = ReSwingValue("Quit")) { //#IS( // )
        clicked += { _ => quit }  //#HDL
      }
    }
    
    contents += new Menu("Edit") {
      contents += undo
    }
    
    contents += new Menu("Tools") {
      contents += merge
    }
  }
  
  val newTabDialog = new NewTabDialog
  
  val serverDialog = new ServerDialog
  
  listenTo(ui.tabbedPane.selection)
  
  reactions += {
    case SelectionChanged(ui.tabbedPane) =>
      drawingSpaceState() =
        if (ui.tabbedPane.selection.index != -1
              && (panelDrawingSpaceStates contains ui.tabbedPane.selection.page))
            panelDrawingSpaceStates(ui.tabbedPane.selection.page)._1
        else
          null
      
      if (ui.tabbedPane.pages.size > 0)
        menu.update()
  }
  
  def addTab(networkSpaceState: DrawingSpaceState => NetworkSpaceState = {_ => null}): Unit = {
    if (newTabDialog.showDialog(ui.locationOnScreen)) {
      val (state, panel) = bilateralValues{ value =>
        lazy val panel = generateDrawingPanel(
            newTabDialog.showIntersections.selected,
            newTabDialog.showCoordinates.selected,
            newTabDialog.showNames.selected,
            state)
        
        lazy val state: DrawingSpaceState = new DrawingSpaceState {
          def isCurrentState(x: Any) = drawingSpaceState.get == this
          
          override lazy val nextShape: Signal[Shape] = Signal { ui.shapeSelectionPanel.nextShape().copy(this) } //#SIG
          override lazy val strokeWidth = Signal { ui.strokeInputPanel.strokeWidth() }  //#SIG
          override lazy val color = Signal { ui.strokeInputPanel.color() }  //#SIG
          
          override lazy val executed: Event[Command] =  //#EVT
            value(panel.drawn || ui.shapePanel.deleted || menu.merged) && isCurrentState _  //#EF //#EF //#EF
          override lazy val reverted: Event[Command] = (ui.commandPanel.revert || //#EVT //#EF
              (menu.undo.clicked map {_: Any => commands.get.head })) && isCurrentState _ //#EF //#EF 
        }
        
        (state, panel)
      }
      addDrawingPanel(panel, networkSpaceState(state))
    }
  }
  
  def generateDrawingPanel(showIntersections: Boolean, showCoordinates: Boolean, showName: Boolean, state: => DrawingSpaceState): DrawingPanel =
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
    menu.update()
  }
  
  def addNetworkTab() {
    if (serverDialog.showDialog(ui.locationOnScreen) && serverDialog.inputIsValid)
      try
        addTab({drawingSpaceState =>
          new NetworkSpaceState(
            drawingSpaceState,
            Swing.onEDTWait,
            serverDialog.hostname,
            serverDialog.commandPort,
            serverDialog.exchangePort,
            serverDialog.listenerPort)})
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
      menu.update()
    }
  }
}