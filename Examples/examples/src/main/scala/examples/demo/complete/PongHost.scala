package examples.demo.complete

import java.awt.Dimension

import examples.demo.JModularMouseBouncingCircle.BouncingCircle
import examples.demo.MBoundFieldCircle.PlayingField
import examples.demo.ORacketBouncingCircle.Racket
import examples.demo.system.Clock
import examples.demo.ui._
import rescala._

import scala.swing.{MainFrame, SimpleSwingApplication, Swing, UIElement}

trait PongHost {
  val shapes: Signal[Traversable[Shape]]
  val fieldWidth: Signal[Int]
  val fieldHeight: Signal[Int]
  def setRacketInput(inputY: Signal[Int]) : Unit
}

object PongHostImpl extends SimpleSwingApplication with PongHost {
  override val shapes = Var(List[Shape]())
  val panel = new ShapesPanel(shapes)
  panel.preferredSize = new Dimension(400, 300)

  override val fieldWidth = panel.width.map(_ - 25)
  override val fieldHeight = panel.height.map(_ - 25)

  val bouncingCircle = new BouncingCircle(Var(50), panel.Mouse.middleButton.pressed)
  val playingField = new PlayingField(fieldWidth, fieldHeight, bouncingCircle.shape)
  val racket = new Racket(fieldWidth, fieldHeight, Var(100), true, panel.Mouse.y)

  bouncingCircle.horizontalBounceSources.transform(playingField.movedOutOfBoundsHorizontal :: racket.collisionWith(bouncingCircle.shape) :: _)
  bouncingCircle.verticalBounceSources.transform(playingField.movedOutOfBoundsVertical :: _)

  shapes.set(List(bouncingCircle.shape, playingField.shape, racket.shape))

  override lazy val top = {
    new MainFrame {
      title = "Pong"
      contents = panel
      setLocationRelativeTo(new UIElement { override def peer = null })
    }
  }

  override def setRacketInput(inputY: Signal[Int]) : Unit = {
    val opponentRacket = new Racket(fieldWidth, fieldHeight, racket.height, false, inputY)
    shapes.transform(opponentRacket.shape :: _)
    bouncingCircle.horizontalBounceSources.transform(opponentRacket.collisionWith(bouncingCircle.shape) :: _)
  }

  override def main(args: Array[String]): Unit = {
    super.main(args)

    while(!top.visible) Thread.sleep(5)

    new PongClient(this).main(Array())

    while(top.visible) {
      Clock.tick()
      Thread.sleep(1)
    }
  }
}

// TODO make this an object with host as a remote.
class PongClient(host: PongHost) extends SimpleSwingApplication {
  val panel = new ShapesPanel(host.shapes)
  host.setRacketInput(panel.Mouse.y)

  override def top = {
    val mf = new MainFrame {
      title = "Player 2"
      contents = panel
    }
    Signal{(host.fieldWidth(), host.fieldHeight())}.observe{ case (w, h) =>
      Swing.onEDT(mf.size = new Dimension(w + 50, h + 60))
    }
    mf
  }
}
