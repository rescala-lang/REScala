package examples.demo

import examples.demo.LFullyModularBall.BouncingBall
import examples.demo.MPlayingFieldBall.PlayingField
import examples.demo.ui.{Shape, ShapesPanel}
import reactives.default.*

/** Because we implemented the collision events inside a method, with each
  * derived from parameters to the method, this method implements a blueprint
  * pattern of several Signals, that can be instantiated multiple times for
  * different input parameters. We exploit this here by wrapping our ball
  * instantiation code into a closure and simply executing it twice,
  * resulting in two balls bouncing around the PlayingField.
  */
object NFieldMultiBall extends Main {
  val shapes = Var[List[Shape]](List.empty)
  val panel  = new ShapesPanel(shapes)

  val playingField = new PlayingField(panel.width.map(_ - 25), panel.height.map(_ - 25))
  shapes.transform(playingField.shape :: _)

  val balls = List(
    new BouncingBall(200d, 150d, Var(50), panel.Mouse.middleButton.pressed),
    new BouncingBall(-200d, 100d, Var(50), panel.Mouse.middleButton.pressed)
  )

  for bouncingBall <- balls do {
    shapes.transform(bouncingBall.shape :: _)

    val fieldCollisions = playingField.colliders(bouncingBall.shape)
    bouncingBall.horizontalBounceSources.transform(fieldCollisions.left :: fieldCollisions.right :: _)
    bouncingBall.verticalBounceSources.transform(fieldCollisions.top :: fieldCollisions.bottom :: _)
  }
}
