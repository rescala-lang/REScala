package examples.demo

import java.awt.event.{ComponentEvent, ComponentListener, MouseAdapter, MouseEvent, MouseWheelEvent}
import java.awt.{Dimension, MouseInfo}

import examples.demo.ui.{Clock, Shape, ShapesPanel}

import scala.swing.{MainFrame, SimpleSwingApplication, UIElement}
import rescala._

abstract class Main extends SimpleSwingApplication {

  def shapes(): List[Shape]
  override lazy val top = {
    new MainFrame {
      title = "Pong"
      contents = panel
      setLocationRelativeTo(new UIElement { override def peer = null })
    }
  }

  val panel = new ShapesPanel()
  panel.preferredSize = new Dimension(400, 300)

  val _dimension: Var[Dimension] = Var(panel.size)
  panel.peer.addComponentListener(new ComponentListener {
    override def componentShown(e: ComponentEvent) = {}
    override def componentHidden(e: ComponentEvent) = {}
    override def componentMoved(e: ComponentEvent) = {}
    override def componentResized(e: ComponentEvent) = _dimension.set(panel.size)
  })

  val panelWidth = _dimension.map(_.width)
  val panelHeight = _dimension.map(_.height)

  case class Point(x: Int, y: Int) {
    def this(from: java.awt.Point) = this(from.x - panel.size.width / 2, from.y - panel.size.height / 2)
  }
  object Mouse {
    class MouseButton {
      val pressed = Evt[Point]
      val released = Evt[Point]
      val clicked = Evt[Point]
      val state = (pressed.map(_ => true) || released.map(_ => false)).latest(false)
    }
    val _position = Var[Point](Point(0, 0))
    val x = _position.map(_.x)
    val y = _position.map(_.y)
    val wheel = Evt[Int]
    val _buttons: Array[MouseButton] = (0 until MouseInfo.getNumberOfButtons).map{_ => new MouseButton}.toArray

    def button(id: Int): MouseButton = _buttons(id - 1)
    val leftButton = button(1)
    val middleButton = button(2)
    val rightButton = button(3)

    val listener = new MouseAdapter {
      override def mousePressed(e: MouseEvent) = button(e.getButton()).pressed.fire(new Point(e.getPoint))
      override def mouseReleased(e: MouseEvent) = button(e.getButton()).released.fire(new Point(e.getPoint))

      override def mouseMoved(e: MouseEvent) = _position.set(new Point(e.getPoint))
      override def mouseDragged(e: MouseEvent) = _position.set(new Point(e.getPoint))

      override def mouseWheelMoved(e: MouseWheelEvent) = wheel.fire(e.getScrollAmount)
    }
    panel.peer.addMouseListener(listener)
    panel.peer.addMouseMotionListener(listener)
    panel.peer.addMouseWheelListener(listener)
  }

  override def main(args: Array[String]): Unit = {
    super.main(args)

    panel.shapes.set(shapes())

    while(!top.visible) Thread.sleep(5)
    while(top.visible) {
      Clock.tick()
      Thread.sleep(1)
    }
  }
}
