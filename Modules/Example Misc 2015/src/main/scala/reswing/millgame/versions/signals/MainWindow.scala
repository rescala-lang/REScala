package reswing.millgame.versions.signals

import java.awt.{BasicStroke, Color, Dimension, Font, RenderingHints}

import reswing.millgame._
import reswing.millgame.types._
import rescala.default._
import reswing._

import scala.swing._
import scala.swing.event._

object MainWindow extends SimpleSwingApplication {
  // uncomment to enable logging:
  // react.ReactiveEngine.log.enableAllLogging

  val game = new MillGame

  def top =
    new MainFrame {
      game.gameWon observe { winner => // #HDL
        Dialog.showMessage(ui, "Game won by " + winner, "Game ended", Dialog.Message.Info)
      }

      game.remainCountChanged observe { count => // #HDL
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

  object ui extends BoxPanel(Orientation.Vertical) {
    val statusBar = new ReLabel(
      text = Signal { game.stateVar().text }, // #SIG
      preferredSize = ReSwingValue(new Dimension(Integer.MAX_VALUE, 64)),
      font = ReSwingValue(new Font("Tahoma", Font.PLAIN, 32))
    )

    val counterBar = new Label("White: 9 / Black: 9") {
      preferredSize = new Dimension(Integer.MAX_VALUE, 32)
      font = new Font("Tahoma", Font.PLAIN, 16)
    }

    contents += statusBar
    contents += counterBar
    contents += new MillDrawer(game)
  }

  def setSystemLookAndFeel(): Unit = {
    import javax.swing.UIManager
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  }
}

class MillDrawer(val game: MillGame) extends ReComponent(preferredSize = new Dimension(500, 500)) {
  val DotRadius     = 5
  val StoneRadius   = 15
  val ClickArea     = 50
  val SizePercent   = 0.8
  val MiddlePercent = 2f / 3
  val InnerPercent  = 1f / 3

  val squareSize: Signal[Int] = Signal { // #SIG
    (math.min(size().width, size().height) * SizePercent).toInt
  }

  val coordinates = Signal { // #SIG
    val midX     = size().width / 2
    val midY     = size().height / 2
    val xFactors = List.fill(3)(List(-1, -1, -1, 0, 1, 1, 1, 0)).flatten
    val yFactors = List.fill(3)(List(-1, 0, 1, 1, 1, 0, -1, -1)).flatten

    for (((xF, yF), i) <- (xFactors zip yFactors).zipWithIndex) yield {
      val distance: Int =
        (i / 8 match {
          case 0 => InnerPercent * squareSize().toFloat
          case 1 => MiddlePercent * squareSize().toFloat
          case 2 => squareSize().toFloat
        }).toInt / 2
      Point(midX + xF * distance, midY + yF * distance)
    }
  }

  val board = Signal { // #SIG
    val offset = (2 * StoneRadius, 2 * StoneRadius)
    val size   = squareSize() + 4 * StoneRadius
    Rect(coordinates()(16) - offset, size, size)
  }

  val lines = Signal { // #SIG
    MillBoardRenamed.lines map { indices =>
      Line(coordinates()(indices.head.index), coordinates()(indices.last.index))
    }
  }

  val selectedIndex = Signal { // #SIG
    game.stateVar() match {
      case MoveStoneDrop(_, index) => index
      case JumpStoneDrop(_, index) => index
      case _                       => SlotIndex(-1)
    }
  }

  val highlightedIndex = Signal { // #SIG
    val index = mouse.moves.moved.holdOption().value match {
      case Some(e) => coordinates() indexWhere {
          p => (p.distance((e.point.x, e.point.y))) < ClickArea
        }
      case _ => -1
    }
    SlotIndex(index)
  }

  val moveLines = Signal { // #SIG
    val possibleMoves =
      if (selectedIndex() == SlotIndex(-1))
        game.possibleNextMoves() filter {
          case (from, to) => from == highlightedIndex() || to == highlightedIndex()
        }
      else
        game.possibleNextMoves() filter (_ == ((selectedIndex(), highlightedIndex()))) match {
          case Nil => game.possibleMoves filter (_._1 == selectedIndex())
          case l   => l
        }

    possibleMoves map {
      case (from, to) =>
        Line(coordinates()(from.index), coordinates()(to.index))
    }
  }

  val indexClicked =
    (mouse.clicks.released map { (e: MouseReleased) => // #EF
      val index = coordinates.value.indexWhere {
        p => (p distance ((e.point.x, e.point.y))) < ClickArea
      }
      SlotIndex(index)
    }) && (_ != SlotIndex(-1)) // #EF

  val backgroundRect = Signal { Rect(0, 0, bounds().width, bounds().height) } // #SIG

  val presentation = Signal { // #SIG
    // background and board
    Seq(
      Presentation(backgroundRect(), color = Color.GRAY),
      Presentation(board(), color = new Color(239, 228, 176))
    ) ++
    // possible moves
    (moveLines() map { Presentation(_, color = new Color(0, 0, 0, 80), width = 5) }) ++
    // lines on the board
    (lines() map { Presentation(_) }) ++
    // dots on the lines
    (coordinates() map { p => Presentation(Circle(p, DotRadius)) }) ++
    // stones
    (game.board.stonesVar().zipWithIndex collect {
      case (color, i) if color != Empty =>
        Presentation(
          Circle(coordinates()(i), StoneRadius),
          color = if (color == Black) Color.BLACK else Color.WHITE
        )
    })
  }

  indexClicked observe { index =>
    game.playerInput(index); ()
  } // #HDL

  presentation.changed observe { _ => this.repaint() } // #HDL

  override def paintComponent(g: Graphics2D): Unit = {
    g.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON
    )

    for (p <- presentation.now)
      p match {
        case Presentation(shape, color, width) =>
          g.setColor(color)
          g.setStroke(new BasicStroke(width.toFloat))

          shape match {
            case Point(x, y) => ()
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
