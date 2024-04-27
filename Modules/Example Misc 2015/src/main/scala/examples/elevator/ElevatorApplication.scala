package examples.elevator

import reactives.default.*

import java.awt.Color
import scala.swing.*

object ElevatorApplication extends SimpleSwingApplication {

  /* Uncomment to enable logging: */
  // react.ReactiveEngine.log.enableAllLogging

  lazy val elevator    = new Elevator(3)
  lazy val application = new ElevatorApplication(elevator)
  def top              = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while (true) {
      Swing onEDTWait { elevator.tick.fire() }
      Thread `sleep` 50
    }
  }
}

class ElevatorApplication(val elevator: Elevator) {
  // drawing code
  val frame = new MainFrame {
    contents = new GridPanel(0, 2) {
      contents += new ElevatorPainter(elevator)
      contents += new GridPanel(elevator.nFloors, 1) {
        for (i <- 0 until elevator.nFloors) {
          vGap = 8
          contents += new Button {
            this.bounds
            text = elevator.nameOfFloor(i)
            reactions += { case _ => elevator.callToFloor.fire(i) }
          }
        }
      }
    }
  }
  elevator.tick observe { _ => frame.repaint() }
}

class ElevatorPainter(e: Elevator) extends Panel {
  val FloorHeight = e.FloorHeight
  val FloorWidth  = (0.9 * e.FloorHeight).toInt
  val sizeX       = FloorWidth + 50
  val sizeY       = FloorHeight * e.nFloors + 50

  preferredSize = new Dimension(sizeX, sizeY)

  override def paintComponent(g: Graphics2D): Unit = {
    draw(g, new Rectangle(0, 0, 0, 0))
  }

  def draw(g: Graphics2D, area: Rectangle): Unit = {
    val FloorX = area.x
    val FloorY = area.y + e.FloorStart

    def drawCart(x: Int, y: Int, w: Int, h: Int): Unit = {
      g.setColor(Color.DARK_GRAY)
      g.fill3DRect(x, y, w, h, true)

      g.setColor(Color.GRAY)
      g.fill3DRect(x + 2, y + 2, w / 2 - 2, h - 4, true)
      g.fill3DRect(x + w / 2 + 2, y + 2, w / 2 - 4, h - 4, true)
    }

    def drawDuct(): Unit = {
      g.setColor(Color.BLACK)
      g.fillRect(FloorX + 5, FloorY + 5, FloorWidth - 10, FloorY + e.nFloors * FloorHeight + 10)
    }

    def drawRoom(y: Int): Unit = {
      g.setColor(new Color(30, 30, 100))
      g.fillRect(FloorX + 8, FloorY + 8 + y, FloorWidth - 16, FloorHeight - 4)
    }

    drawDuct()
    e.FloorPos.foreach(drawRoom(_))
    val pos = e.position.now
    drawCart(FloorX + 8, FloorY + 8 + pos, FloorWidth - 16, FloorHeight - 4)
  }
}
