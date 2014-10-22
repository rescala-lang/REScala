package animal.run

import swing._
import java.awt.{ Color, Graphics2D, Dimension }
import java.awt.Point
import scala.swing.Swing
import javax.imageio.ImageIO
import java.io.File
import scala.collection.mutable.HashMap

// chose version here
//import animal.versions.observer.World
//import animal.versions.event.World
//import animal.versions.signalonly.World
import animal.versions.signal.World

object RunSwingWindow extends SimpleSwingApplication {

  val sleeptime = 40
  val nAnimals = 8
  val nPlants = 8

  override def main(args: Array[String]): Unit = {
    super.main(args)
    //rescala.ReactiveEngine.log.enableDefaultLogging

    val world = new World
    world batchSpawn (nAnimals, nPlants)
    
    while (true) {
      world.tick()
      // gui
      statusBar.text = world.status
      frame.boardString = world.dump
      frame.repaint      
      Thread sleep sleeptime
    }
  }

  object Images {
    val width = 40
    val height = 40
    val imageNames = Map(
      "." -> "img/empty.png",
      "#" -> "img/plant.png",
      "F" -> "img/female-sheep.png",
      "f" -> "img/female-sheep.png",
      "m" -> "img/male-sheep.png",
      "x" -> "img/little-sheep.png")

    val images = new HashMap[String, java.awt.Image]
    for ((sym, path) <- imageNames) 
      images(sym) = ImageIO.read(new File(path))

    def get(symbol: String) = images(symbol)
  }
  
   val statusBar = new Label {
      preferredSize = new Dimension(32, 32)
      font = new Font("Tahoma", java.awt.Font.PLAIN, 16)
   }

  // drawing code
  def top = frame
  val frame = new MainFrame {
    var boardString = ""

    val panel = new Panel() {
      preferredSize = new Dimension(1200, 600)
      override def paintComponent(g: Graphics2D): Unit = {

        boardString.lines.zipWithIndex.foreach {
          case (line, lineIndex) =>
            line.zipWithIndex.foreach {
              case (char, colIndex) =>
                g.drawImage(
                    Images.get(char.toString),
                    Images.width * colIndex,
                    Images.height * lineIndex, null)
            }
        }
      }
    }
    
    contents = new BoxPanel(Orientation.Vertical) {
      contents += statusBar
      contents += panel
    }
  }
}
