package examples.fisheye

import java.awt.{Color, Dimension, Graphics2D, Rectangle}

import examples.Mouse
import rescala.default._

import scala.swing.{MainFrame, Panel, SimpleSwingApplication}

object Fisheye extends SimpleSwingApplication {
  lazy val application = new Fisheye
  def top              = application.frame
}

object Box {
  val NormalSize = 50
  val HoverSize  = 80
  val DeltaSize  = HoverSize - NormalSize
  val YPos       = 20
  val Margin     = 10
}

class Box(val color: java.awt.Color, val xOffset: Signal[Int])(implicit val mouse: Mouse) {

  private def interpolation(d: Double) = math.max(0, math.min(1, 5 - math.log(d)))

  val lowerLeft          = Signal { new java.awt.Point(xOffset() + Box.Margin, Box.YPos) }
  val mouseDistance      = Signal { mouse.position().distance(lowerLeft()) }
  val interpolationValue = Signal { interpolation(mouseDistance()) }
  val effectiveSize      = Signal { (Box.NormalSize + interpolationValue() * Box.DeltaSize).toInt }
  val rightmostPoint     = Signal { lowerLeft().getX.toInt + effectiveSize() }

  // add some saturation
  val components     = color.getRGBColorComponents(null).map(_.toInt * 255)
  val hsv            = Color.RGBtoHSB(components(0), components(1), components(2), null)
  val effectiveColor = Signal { Color.getHSBColor(hsv(0), 0.6f + 0.4f * interpolationValue().toFloat, hsv(2)) }

  // define the box
  val area = Signal {
    new Rectangle(xOffset(), Box.YPos, effectiveSize(), effectiveSize())
  }
}

class Fisheye {

  val Max_X     = 500
  val Max_Y     = 200
  val initPoint = Signal { 30 }

  implicit val mouse: Mouse = new Mouse

  var boxes: List[Box] = Nil
  def addBox(color: java.awt.Color): Unit = {
    boxes ::= new Box(color, if (boxes.isEmpty) initPoint else boxes.head.rightmostPoint)
  }

  addBox(Color.BLUE)
  addBox(Color.CYAN)
  addBox(Color.GREEN)
  addBox(Color.YELLOW)
  addBox(Color.RED)
  addBox(Color.MAGENTA)

  // redraw code
  mouse.position.changed += { _ => frame.repaint() }

  // drawing code
  val frame: MainFrame = new MainFrame {
    title = "Fisheye boxes"
    resizable = false
    contents = new Panel() {

      /** forward mouse events to EScala wrapper class. To replace once reactive GUI lib is complete */
      listenTo(this.mouse.moves, this.mouse.clicks)
      reactions += Fisheye.this.mouse.react

      preferredSize = new Dimension(Max_X, Max_Y)
      // val scoreFont = new Font("Tahoma", java.awt.Font.PLAIN, 32)
      override def paintComponent(g: Graphics2D): Unit = {
        for (box <- boxes) {
          g.setColor(box.effectiveColor.now)
          g.fill(box.area.now)
        }
      }
    }
  }
}
