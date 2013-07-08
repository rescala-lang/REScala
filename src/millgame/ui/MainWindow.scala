package millgame.ui


/// Chose version here:
//import millgame.versions.events.MillGame
import millgame.versions.signals.MillGame

import millgame.types._
import millgame.types.Pos
import millgame.types.Pos._
import java.awt.Dimension
import swing._
import javax.swing.UIManager
import scala.swing.event.MouseClicked
import scala.swing.event.MouseReleased


object MainWindow extends SimpleSwingApplication {

  var game = new MillGame
  
  game.stateChanged += { state =>
    ui.statusBar.text = state.text
  }
  
  game.gameWon += { winner =>
     Dialog.showMessage(ui, "Game won by " + winner, "Game ended", Dialog.Message.Info)
  }
  
  game.remainCountChanged += { count =>
    ui.counterBar.text = 
    "White: " + count(White) + " / " +
    "Black: " + count(Black)
  }
  
  override def main(args: Array[String]) {
    super.main(args)
  }

  val top = new MainFrame {
    setSystemLookAndFeel()
    minimumSize = new Dimension(400, 400)
    preferredSize = new Dimension(800, 600)
    title = "Nine Men's Mill game"
    contents = ui
  }

  
  lazy val ui = new BoxPanel(Orientation.Vertical) {

    val statusBar = new Label(game.stateText) {
      preferredSize = new Dimension(Integer.MAX_VALUE, 64)
      font = new Font("Tahoma", java.awt.Font.PLAIN, 32)
    }
    
    val counterBar = new Label("White: 9 / Black: 9") {
      preferredSize = new Dimension(Integer.MAX_VALUE, 32)
      font = new Font("Tahoma", java.awt.Font.PLAIN, 16)
    }
    contents += statusBar
    contents += counterBar
    contents += new MillDrawer
  }


  def setSystemLookAndFeel() {
    import javax.swing.UIManager
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  }
}


class MillDrawer extends Component {
  preferredSize = new Dimension(500, 500)
  
  val DotSize = 10
  val StoneSize = 30
  val ClickArea = 50
  val SizePercent = 0.8
  val MiddlePercent = 2f / 3
  val InnerPercent = 1f / 3
  
  var squareSize = 0
  var coordinates = Array.fill(24)((0,0))
  
  def computeCoordinates {
    val smaller = math.min(size.width, size.height)
    squareSize = (smaller * SizePercent).toInt  
    
    val midX = size.width / 2
    val midY = size.height / 2    
    val xFactors = List.make(3, List(-1, -1, -1, 0, 1, 1, 1,0)).flatten
    val yFactors = List.make(3, List(-1, 0, 1, 1, 1, 0, -1, -1)).flatten
    for((i, (xF, yF)) <- coordinates.indices.zip(xFactors.zip(yFactors)))
    {
      val distance = (i / 8 match {
        case 0 => InnerPercent * squareSize
        case 1 => MiddlePercent * squareSize
        case 2 => squareSize
      }).toInt / 2
      coordinates(i) = (midX + xF * distance, midY + yF * distance)
    }
  }    
    
  override def paintComponent(g: java.awt.Graphics2D) {
      computeCoordinates
      
      g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
      
      // background
      g.setColor(java.awt.Color.GRAY)
      g.fillRect(0, 0, bounds.width, bounds.height)
      
      // draw board
      g.setColor(new java.awt.Color(239, 228, 176))
      g.fill3DRect(coordinates(16)._1 - StoneSize, coordinates(16)._2 - StoneSize, squareSize + StoneSize*2, squareSize + StoneSize*2 ,true)
            
      // draw lines
      g.setColor(java.awt.Color.BLACK)
      g.drawRect(coordinates(16)._1, coordinates(16)._2, squareSize, squareSize)
      g.drawRect(coordinates(8)._1, coordinates(8)._2, (squareSize * MiddlePercent).toInt, (squareSize * MiddlePercent).toInt)
      g.drawRect(coordinates(0)._1, coordinates(0)._2, (squareSize * InnerPercent).toInt, (squareSize * InnerPercent).toInt)
      for(i <- 1 to 8 by 2){
        val (startX, startY) = coordinates(i)
        val (endX, endY) = coordinates(i + 16)
        g.drawLine(startX, startY, endX, endY)
      }
      
      // draw dots
      coordinates.foreach {case (x,y) => g.fillOval(x - DotSize/2, y - DotSize/2, DotSize, DotSize)}
      
      // draw stones
      MainWindow.game.board.stones.zipWithIndex.foreach {
        case (color, i) if color != Empty =>
          val (x, y) = coordinates(i)
          g.setColor(color match { case Black => java.awt.Color.BLACK; case White => java.awt.Color.WHITE })
          g.fillOval(x - StoneSize / 2, y - StoneSize / 2, StoneSize, StoneSize)
        case _ =>
      }
  }
  
  listenTo(mouse.clicks)
    reactions += {
      case e: MouseReleased =>
        val clickedIndex = coordinates.indexWhere(point => Pos(e.point.x, e.point.y).distance(point) < ClickArea)
        
        if(clickedIndex >= 0){
          MainWindow.game.playerInput(clickedIndex)
          repaint
        }
    }
}