package ex201x.programming2016demo

import ex201x.programming2016demo.LFullyModularBall.BouncingBall
import ex201x.programming2016demo.MPlayingFieldBall.PlayingField
import ex201x.programming2016demo.ORacketMultiBall.Racket
import ex201x.programming2016demo.ui.{Shape, ShapesPanel}
import reactives.default.*
import reactives.structure.Pulse

import java.awt.event.KeyEvent
import scala.swing.{Dimension, MainFrame, SimpleSwingApplication}
import scala.util.Try

object RRecovery extends Main {
  class Opponent(panelSize: Signal[Dimension], shapes: Signal[List[Shape]]) extends SimpleSwingApplication {
    val panel2 = new ShapesPanel(shapes)

    override lazy val top = new MainFrame {
      title = "Player 2"
      contents = panel2
      resizable = false
      override def closeOperation(): Unit = {
        val s = panel2.Mouse._position
        transaction(s) { t ?=>
          s.admitPulse(Pulse.Exceptional(new IllegalArgumentException))(using t)
        }

      }
    }

    panelSize.observe { d =>
      panel2.preferredSize = d
      top.pack()
    }
  }
  val shapes         = Var[List[Shape]](List.empty)
  val filteredShapes = Signal.dynamic { shapes.value.filter { q => Try(q.changed.value).isSuccess } }
  filteredShapes.observe(shapes.set)
  val panel = new ShapesPanel(filteredShapes)

  val playingField = new PlayingField(panel.width.map(_ - 25), panel.height.map(_ - 25))
  val racket       = new Racket(playingField.width, true, playingField.height, panel.Mouse.y)
  shapes.transform(playingField.shape :: racket.shape :: _)

  val balls = List(
    new BouncingBall(200d, 150d, Var(50), panel.Mouse.middleButton.pressed),
    new BouncingBall(-200d, 100d, Var(50), panel.Mouse.middleButton.pressed)
  )

  for bouncingBall <- balls do {
    shapes.transform(bouncingBall.shape :: _)

    val fieldCollisions = playingField.colliders(bouncingBall.shape)
    bouncingBall.horizontalBounceSources.transform(fieldCollisions.left :: fieldCollisions.right :: _)
    bouncingBall.verticalBounceSources.transform(fieldCollisions.top :: fieldCollisions.bottom :: _)

    val racketCollision = racket.collisionWith(bouncingBall.shape)
    bouncingBall.horizontalBounceSources.transform(racketCollision :: _)
  }

  def addOpponent() = {

    val opponent = new Opponent(panel.sigSize, filteredShapes)
    opponent.main(Array())
    val racket2 = new Racket(playingField.width, false, playingField.height, opponent.panel2.Mouse.y)
    shapes.transform(racket2.shape :: _)
    racket2.posY.observe(
      _ => (),
      { _ =>
        shapes.transform(_.filter(_ != racket2.shape))
      }
    )

    for bouncingBall <- balls do {
      bouncingBall.horizontalBounceSources.transform(racket2.collisionWith(bouncingBall.shape) :: _)
    }
  }

  panel.Keyboard.released.map(_.getKeyChar).observe {
    case KeyEvent.VK_ENTER => addOpponent()
    case _                 =>
  }
}
