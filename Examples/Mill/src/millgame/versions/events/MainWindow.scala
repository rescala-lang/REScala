package millgame.versions.events

import millgame._
import millgame.types._
import millgame.types.Point

import makro.SignalMacro.{SignalM => Signal}

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import java.awt.RenderingHints
import swing.event._
import swing._
import reswing._

import scala.Numeric.Implicits._

object MainWindow extends SimpleSwingApplication {
  // uncomment to enable logging:
  // react.ReactiveEngine.log.enableAllLogging
  
  val game = new MillGame
  
  def top = new MainFrame {
    game.gameWon += { winner => //#HDL
       Dialog.showMessage(ui, "Game won by " + winner, "Game ended", Dialog.Message.Info)
    }
    
    game.remainCountChanged += { count => //#HDL
      ui.counterBar.text =
        "White: " + count(White) + " / " +
        "Black: " + count(Black)
    }
    
    setSystemLookAndFeel()
    minimumSize = new Dimension(400, 400)
    preferredSize = new Dimension(800, 600)
    title = "Nine Men's Mill game"
    contents = ui
  }
  
  lazy val ui = new BoxPanel(Orientation.Vertical) {
    val statusBar = new Label {
      text = game.state.text
      preferredSize = new Dimension(Integer.MAX_VALUE, 64)
      font = new Font("Tahoma", Font.PLAIN, 32)
    }
    
    game.stateChanged += { state => //#HDL
      statusBar.text = state.text
    }
    
    val counterBar = new Label("White: 9 / Black: 9") {
      preferredSize = new Dimension(Integer.MAX_VALUE, 32)
      font = new Font("Tahoma", Font.PLAIN, 16)
    }
    
    contents += statusBar
    contents += counterBar
    contents += new MillDrawer(game)
  }
  
  def setSystemLookAndFeel() {
    import javax.swing.UIManager
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  }
}

class MillDrawer(val game: MillGame)
    extends ReComponent(preferredSize = new Dimension(500, 500)) {
  val DotRadius = 5
  val StoneRadius = 15
  val ClickArea = 50
  val SizePercent = 0.8
  val MiddlePercent = 2f / 3
  val InnerPercent = 1f / 3
  
  var coordinates = List.empty[Point[Int]]
  var board = Rect(0, 0, 0, 0)
  var lines = Seq.empty[Line[Int]]
  var moveLines = Seq.empty[Line[Int]]
  var backgroundRect = Rect(0, 0, 0, 0)
  
  var selectedIndex = SlotIndex(-1)
  var highlightedIndex = SlotIndex(-1)
  
  var presentation = Seq.empty[Presentation[Int, Shape[Int]]]
  
  size.changed += { size => //#HDL
    // square size
    val squareSize = (math.min(size.width, size.height) * SizePercent).toInt
    
    // coordinates
    val midX = size.width / 2
    val midY = size.height / 2
    val xFactors = List.fill(3)(List(-1, -1, -1, 0, 1, 1, 1, 0)).flatten
    val yFactors = List.fill(3)(List(-1, 0, 1, 1, 1, 0, -1, -1)).flatten
    
    coordinates =
      for(((xF, yF), i) <- (xFactors zip yFactors).zipWithIndex) yield {
        val distance = (i / 8 match {
          case 0 => InnerPercent * squareSize
          case 1 => MiddlePercent * squareSize
          case 2 => squareSize
        }).toInt / 2
        Point(midX + xF * distance, midY + yF * distance)
      }
    
    // board
    val boardOffset = (2 * StoneRadius, 2 * StoneRadius)
    val boardSize = squareSize + 4 * StoneRadius
    
    board = Rect(coordinates(16) - boardOffset, boardSize, boardSize)
    
    // lines
    lines = MillBoard.lines map { indices =>
      Line(coordinates(indices.head.index), coordinates(indices.last.index)) }
    
    updatePresentation
  }
  
  game.stateChanged += { state => //#HDL
    selectedIndex = (state match {
      case MoveStoneDrop(_, index) => index
      case JumpStoneDrop(_, index) => index
      case _ => SlotIndex(-1)
    })
    updateMoveLines
  }
  
  mouse.moves.moved += { e => //#HDL
    val index = coordinates indexWhere {
      p => (p distance (e.point.x, e.point.y)) < ClickArea
    }
    highlightedIndex = SlotIndex(index)
    updateMoveLines
  }
  
  def updateMoveLines {
    val possibleMoves =
      if (selectedIndex == SlotIndex(-1))
        game.possibleMoves filter {
          case (from, to) => from == highlightedIndex || to == highlightedIndex
        }
      else
        game.possibleMoves filter (_ == (selectedIndex, highlightedIndex)) match {
          case Nil => game.possibleMoves filter (_._1 == selectedIndex)
          case l => l
        }
    
    moveLines = possibleMoves map { case(from, to) =>
      Line(coordinates(from.index), coordinates(to.index)) }
    
    updatePresentation
  }
  
  val indexClicked = (mouse.clicks.released map { e: MouseReleased => //#EF
    val index = coordinates.indexWhere {
      p => (p distance (e.point.x, e.point.y)) < ClickArea
    }
    SlotIndex(index)
  }) && (_ != SlotIndex(-1)) //#EF
  
  indexClicked += { index => game.playerInput(index) } //#HDL
  
  bounds.changed += { bounds =>
    backgroundRect = Rect(0, 0, bounds.width, bounds.height)
    updatePresentation
  }
  
  def updatePresentation { //#HDL
    presentation =
      // background and board
      Seq(
        Presentation(backgroundRect, color = Color.GRAY),
        Presentation(board, color = new Color(239, 228, 176))
      ) ++
      // possible moves
      (moveLines map { Presentation(_, color = new Color(0, 0, 0, 80), width = 5) }) ++
      // lines on the board
      (lines map { Presentation(_) }) ++
      // dots on the lines
      (coordinates map { p => Presentation(Circle(p, DotRadius)) }) ++
      // stones
      (game.board.stones.zipWithIndex collect {
        case (color, i) if color != Empty =>
          Presentation(
            Circle(coordinates(i), StoneRadius),
            color = if (color == Black) Color.BLACK else Color.WHITE)
      })
    
    this.repaint
  }
  
  game.board.numStonesChanged += { _ => updatePresentation }
  
  override def paintComponent(g: Graphics2D) {
    g.setRenderingHint(
        RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    
    for (p <- presentation)
      p match {
        case Presentation(shape, color, width) =>
          g.setColor(color)
          g.setStroke(new BasicStroke(width))
          
          shape match {
            case Line(from, to) =>
              g.drawLine(from.x, from.y, to.x, to.y)
            case Rect(anchor, width, height) =>
              g.fillRect(anchor.x, anchor.y, width, height)
            case Circle(center, radius) =>
              g.fillOval(center.x - radius, center.y - radius, 2 * radius, 2 * radius)
          }
      }
  }
}
