package examples.demo

import java.awt.event.{ComponentEvent, ComponentListener, MouseAdapter, MouseEvent, MouseWheelEvent}
import java.awt.{Dimension, MouseInfo}

import examples.demo.system.Clock
import examples.demo.ui.{Shape, ShapesPanel}

import scala.swing.{MainFrame, SimpleSwingApplication, UIElement}
import rescala._

abstract class Main extends SimpleSwingApplication {

  def makeShapes(): List[Shape]
  override lazy val top = {
    new MainFrame {
      title = "Pong"
      contents = panel
      setLocationRelativeTo(new UIElement { override def peer = null })
    }
  }

  val shapes = Var(List[Shape]())
  val panel = new ShapesPanel(shapes)
  panel.preferredSize = new Dimension(400, 300)

  override def main(args: Array[String]): Unit = {
    super.main(args)

    shapes.set(makeShapes())

    while(!top.visible) Thread.sleep(5)
    while(top.visible) {
      Clock.tick()
      Thread.sleep(1)
    }
  }
}
