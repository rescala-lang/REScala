package examples.demo.complete

import java.awt.Dimension

import examples.demo.GModularClockCircle.Clock
import examples.demo.LFullyModularBall.BouncingBall
import examples.demo.MPlayingFieldBall.PlayingField
import examples.demo.ORacketMultiBall.Racket
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
  val shapes = Var[List[Shape]](List.empty)
  val panel = new ShapesPanel(shapes)

  val fieldWidth = panel.width.map(_ - 25)
  val fieldHeight = panel.height.map(_ - 25)

  val playingField = new PlayingField(fieldWidth, fieldHeight)
  val racket = new Racket(fieldWidth, true, fieldHeight, panel.Mouse.y)
  shapes.transform(playingField.shape :: _)

  val bouncingBall = new BouncingBall(200d, 150d, Var(50), panel.Mouse.middleButton.pressed)
  shapes.transform(bouncingBall.shape :: _)

  val fieldCollisions = playingField.colliders(bouncingBall.shape)
  bouncingBall.horizontalBounceSources.transform(fieldCollisions.left :: fieldCollisions.right :: _)
  bouncingBall.verticalBounceSources.transform(fieldCollisions.top :: fieldCollisions.bottom :: _)

  val racketCollision = racket.collisionWith(bouncingBall.shape)
  bouncingBall.horizontalBounceSources.transform(racketCollision :: _)

  override val top = {
    panel.preferredSize = new Dimension(400, 300)
    new MainFrame {
      title = "REScala Demo"
      contents = panel
      setLocationRelativeTo(new UIElement { override def peer = null })
    }
  }

  override def setRacketInput(inputY: Signal[Int]) : Unit = {
    val opponentRacket = new Racket(fieldWidth, false, fieldHeight, inputY)
    shapes.transform(opponentRacket.shape :: _)
    bouncingBall.horizontalBounceSources.transform(opponentRacket.collisionWith(bouncingBall.shape) :: _)
  }

  override def main(args: Array[String]): Unit = {
    super.main(args)

    while(!top.visible) Thread.sleep(5)

    new PongClient(this).main(Array())

    while(top.visible) {
      Thread.sleep(1)
      Clock.tick()
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
