package reswing.reshapes

import reactives.default.*
import reswing.ReMenuItem.toMenuItem
import reswing.reshapes.actions.{LoadAction, SaveAction}
import reswing.reshapes.drawing.{Command, DrawingSpaceState, MergeDrawingSpaces, NetworkSpaceState}
import reswing.reshapes.figures.Shape
import reswing.reshapes.ui.dialogs.{NewTabDialog, ServerDialog}
import reswing.reshapes.ui.panels.{CommandPanel, DrawingPanel, InfoPanel, ShapePanel, ShapeSelectionPanel, ShowCoordinateSystem, ShowIntersection, ShowNameLabels, StrokeInputPanel}
import reswing.reshapes.util.ReactiveUtil.{UnionEvent, bilateralValues}
import reswing.{ReMenu, ReMenuItem, ReSwingValue}

import java.net.{BindException, ConnectException}
import javax.swing.JOptionPane
import scala.collection.mutable.HashMap
import scala.swing.BorderPanel.Position
import scala.swing.TabbedPane.Page
import scala.swing.event.SelectionChanged
import scala.swing.{Action, BorderPanel, Component, Dimension, MainFrame, Menu, MenuBar, MenuItem, Separator, SimpleSwingApplication, Swing, TabbedPane}

object ReShapes extends SimpleSwingApplication {
  private val panelDrawingSpaceStates = new HashMap[TabbedPane.Page, (DrawingSpaceState, NetworkSpaceState)]

  val drawingSpaceState = Var[DrawingSpaceState](null) // #VAR

  def top =
    new MainFrame {
      title = "ReShapes"
      preferredSize = new Dimension(1000, 600)
      menuBar = menu
      contents = ui
    }

  object ui extends BorderPanel {
    val tabbedPane          = new TabbedPane
    val strokeInputPanel    = new StrokeInputPanel
    val shapeSelectionPanel = new ShapeSelectionPanel
    val shapePanel          = new ShapePanel
    val commandPanel        = new CommandPanel

    layout(tabbedPane) = Position.Center
    layout(strokeInputPanel) = Position.North
    layout(new InfoPanel) = Position.South
    layout(shapeSelectionPanel) = Position.West
    layout(new TabbedPane {
      pages += new TabbedPane.Page("Shapes", shapePanel)
      pages += new TabbedPane.Page("Commands", commandPanel)
    }) = Position.East
  }

  object menu extends MenuBar {
    val undo = new ReMenuItem(
      "Undo",
      enabled = Signal.dynamic { // #SIG //#IS( // )
        drawingSpaceState.value != null && drawingSpaceState.value.commands.value.nonEmpty
      }
    )

    val merge = new ReMenu(
      text = "Merge with...", // #SIG //#IS( // )
      contents = Signal {     // #SIG //#IS( // )
        itemsEvents.value map { case (btn, _) => btn }
      }
    )

    final lazy val merged = UnionEvent(Signal { // #SIG //#UE( //#EVT //#IF )
      itemsEvents.value map { case (_, ev) => ev }
    })

    lazy val update = Evt[Unit]() // #EVT

    private lazy val itemsEvents: Signal[Seq[(Component, Event[Command])]] = // #SIG
      (update map { (_: Any) => // #EF
        (ui.tabbedPane.pages filter { tab => tab.index != ui.tabbedPane.selection.index } map { (tab: Page) =>
          val item = new ReMenuItem(tab.title) // #IS( // )
          val command = item.clicked map { (_: Any) => // #EF
            new MergeDrawingSpaces(panelDrawingSpaceStates(tab)._1)
          }
          (item: Component, command)
        }).toSeq
      }) `hold` Seq.empty // #IF

    contents += new Menu("File") {
      contents += new MenuItem(Action("New tab") { addTab() })
      contents += new MenuItem(Action("New network tab") { addNetworkTab() })
      contents += new MenuItem(Action("Remove selected tab") { removeCurrentTab() })
      contents += new Separator
      contents += new MenuItem(new SaveAction)
      contents += new MenuItem(new LoadAction)
      contents += new Separator
      contents += new ReMenuItem(text = ReSwingValue("Quit")) { // #IS( // )
        clicked observe { _ => quit() } // #HDL
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
      drawingSpaceState.set(
        if
          ui.tabbedPane.selection.index != -1
          && (panelDrawingSpaceStates contains ui.tabbedPane.selection.page)
        then
          panelDrawingSpaceStates(ui.tabbedPane.selection.page)._1
        else
          null
      )

      if ui.tabbedPane.pages.size > 0 then
        menu.update.fire()
  }

  def addTab(networkSpaceState: DrawingSpaceState => NetworkSpaceState = { _ => null }): Unit = {
    if newTabDialog.showDialog(ui.locationOnScreen) then {
      val (state, panel) = bilateralValues { value =>
        lazy val panel = generateDrawingPanel(
          newTabDialog.showIntersections.selected,
          newTabDialog.showCoordinates.selected,
          newTabDialog.showNames.selected,
          state
        )

        lazy val state: DrawingSpaceState = new DrawingSpaceState {
          def isCurrentState = drawingSpaceState.now == this

          override lazy val nextShape: Signal[Shape] =
            Signal { ui.shapeSelectionPanel.nextShape.value.copy(this) } // #SIG
          override lazy val strokeWidth = Signal { ui.strokeInputPanel.strokeWidth.value } // #SIG
          override lazy val color       = Signal { ui.strokeInputPanel.color.value }       // #SIG

          override lazy val executed: Event[Command] = // #EVT
            value(panel.drawn || ui.shapePanel.deleted || menu.merged) && (_ => isCurrentState) // #EF //#EF //#EF
          override lazy val reverted: Event[Command] = value(ui.commandPanel.revert || // #EVT //#EF
            (menu.undo.clicked map { (_: Any) => commands.value.head })) && (_ => isCurrentState) // #EF //#EF
        }

        (state, panel)
      }
      addDrawingPanel(panel, networkSpaceState(state))
    }
  }

  def generateDrawingPanel(
      showIntersections: Boolean,
      showCoordinates: Boolean,
      showName: Boolean,
      state: => DrawingSpaceState
  ): DrawingPanel =
    (showIntersections, showCoordinates, showName) match {
      case (true, false, false) => new DrawingPanel(state) with ShowIntersection
      case (false, true, false) => new DrawingPanel(state) with ShowCoordinateSystem
      case (true, true, false)  => new DrawingPanel(state) with ShowIntersection with ShowCoordinateSystem
      case (false, false, true) => new DrawingPanel(state) with ShowNameLabels
      case (true, false, true)  => new DrawingPanel(state) with ShowIntersection with ShowNameLabels
      case (true, true, true) =>
        new DrawingPanel(state) with ShowIntersection with ShowCoordinateSystem with ShowNameLabels
      case _ => new DrawingPanel(state)
    }

  def addDrawingPanel(panel: DrawingPanel, networkSpaceState: NetworkSpaceState): Unit = {
    val page = new TabbedPane.Page("drawing#%d".format(ui.tabbedPane.pages.size + 1), panel)
    panelDrawingSpaceStates(page) = (panel.state, networkSpaceState)
    ui.tabbedPane.pages += page
    menu.update.fire()
  }

  def addNetworkTab(): Unit = {
    if serverDialog.showDialog(ui.locationOnScreen) && serverDialog.inputIsValid() then
      try addTab({ drawingSpaceState =>
          new NetworkSpaceState(
            drawingSpaceState,
            Swing.onEDTWait,
            serverDialog.hostname,
            serverDialog.commandPort,
            serverDialog.exchangePort,
            serverDialog.listenerPort
          )
        })
      catch {
        case e: ConnectException =>
          JOptionPane.showMessageDialog(null, "Server not available", "ConnectException", JOptionPane.ERROR_MESSAGE)
        case e: BindException =>
          JOptionPane.showMessageDialog(null, "Port cannot be bound", "BindException", JOptionPane.ERROR_MESSAGE)
        case e: Exception =>
          e.printStackTrace()
          JOptionPane.showMessageDialog(null, "Invalid input!")
          addNetworkTab()
      }
  }

  def removeCurrentTab(): Unit = {
    if ui.tabbedPane.pages.size > 0 then {
      val (_, networkSpaceState) = panelDrawingSpaceStates(ui.tabbedPane.selection.page)
      if networkSpaceState != null then
        networkSpaceState.dispose()
      panelDrawingSpaceStates remove ui.tabbedPane.selection.page
      ui.tabbedPane.pages remove ui.tabbedPane.selection.index
      menu.update.fire()
    }
  }
}
