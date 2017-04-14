package examples.demo

import java.rmi.server.UnicastRemoteObject
import java.rmi.{Naming, Remote, RemoteException}
import javax.swing.JOptionPane

import examples.demo.LFullyModularBall.BouncingBall
import examples.demo.MPlayingFieldBall.PlayingField
import examples.demo.ORacketMultiBall.Racket
import examples.demo.ui._
import rescala._
import rescala.rmi.RemoteReactives

import scala.swing.{Dimension, MainFrame, SimpleSwingApplication}

object Opponent extends SimpleSwingApplication {
  val hostUrl = JOptionPane.showInputDialog(null, "Please enter host address.", "Connect", JOptionPane.QUESTION_MESSAGE, null, null, "localhost").toString
  val panelSize: Signal[Dimension] = RemoteReactives.lookupSignal(hostUrl+"/displaySize")
  val shapes: Signal[List[Shape]] = RemoteReactives.lookupSignal(hostUrl+"/gameState")
  val panel2 = new ShapesPanel(shapes)
  Naming.lookup("rmi://"+hostUrl+"/opponentRegistry").asInstanceOf[QRemoteRacketBall.OpponentRegistry].setOpponentInput(panel2.Mouse.y)
  override lazy val top = new MainFrame{
    title = "Player 2"
    contents = panel2
    resizable = false
  }
  panelSize.observe { d =>
    panel2.preferredSize = d
    top.pack()
  }
}

object QRemoteRacketBall extends Main {
  val shapes = Var[List[Shape]](List.empty)
  val panel = new ShapesPanel(shapes)
  RemoteReactives.requireRegistry()
  RemoteReactives.rebind("displaySize", panel.sigSize)
  RemoteReactives.rebind("gameState", shapes)

  val playingField = new PlayingField(panel.width.map(_ - 25), panel.height.map(_ - 25))
  val racket = new Racket(playingField.width, true, playingField.height, panel.Mouse.y)
  shapes.transform(playingField.shape :: racket.shape :: _)

  val balls = List(
    new BouncingBall(200d, 150d, Var(50), panel.Mouse.middleButton.pressed),
    new BouncingBall(-200d, 100d, Var(50), panel.Mouse.middleButton.pressed))

  for(bouncingBall <- balls) {
    shapes.transform(bouncingBall.shape :: _)

    val fieldCollisions = playingField.colliders(bouncingBall.shape)
    bouncingBall.horizontalBounceSources.transform(fieldCollisions.left :: fieldCollisions.right :: _)
    bouncingBall.verticalBounceSources.transform(fieldCollisions.top :: fieldCollisions.bottom :: _)

    val racketCollision = racket.collisionWith(bouncingBall.shape)
    bouncingBall.horizontalBounceSources.transform(racketCollision :: _)
  }

  trait OpponentRegistry extends Remote {
    @throws[RemoteException] def setOpponentInput(inputY: Signal[Int]): Unit
  }
  object regImpl extends UnicastRemoteObject with OpponentRegistry {
    override def setOpponentInput(inputY: rescala.Signal[Int]): Unit = {
      val racket2 = new Racket(playingField.width, false, playingField.height, inputY)
      shapes.transform(racket2.shape :: _)
      for(bouncingBall <- balls) {
        bouncingBall.horizontalBounceSources.transform(racket2.collisionWith(bouncingBall.shape) :: _)
      }
    }
  }
  Naming.rebind("opponentRegistry", regImpl)
}
