package examples.demo

import java.awt.Dimension

import examples.demo.ui.ShapesPanel

import scala.swing.{MainFrame, SimpleSwingApplication, UIElement}

abstract class Main extends SimpleSwingApplication {
  val panel: ShapesPanel

  override val top = {
    panel.preferredSize = new Dimension(400, 300)
    new MainFrame {
      title = "REScala Demo"
      contents = panel
      setLocationRelativeTo(new UIElement { override def peer = null })
    }
  }

  override def main(args: Array[String]): Unit = {
    super.main(args)

    while(!top.visible) Thread.sleep(5)
    while(top.visible) {
      Thread.sleep(1)
      tick()
    }
  }
}
