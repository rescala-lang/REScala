package ex201x.programming2016demo

import ex201x.programming2016demo.LFullyModularBall.BouncingBall
import ex201x.programming2016demo.MPlayingFieldBall.PlayingField
import ex201x.programming2016demo.ui.{Rectangle, Shape, ShapesPanel}
import reactives.default.*

import java.awt.Color

/** Lastly, we implement a Racket module, that implements a Rectangle
  * positioned on either the left or right side of the field, and moving
  * vertically to a desired position, but bound by the playing field height.
  * We instantiate one for the left side, controlled by the Mouse.y position,
  * and add it to the list of displayed shapes. To also support multiple
  * balls, we again implement its collision computation as blueprint
  * derivations inside a method. We add an according instantiation into
  * the ball initialization closure, adding the collision event as a
  * horizontal bounce source.
  */
object ORacketMultiBall extends Main {
  class Racket(
      val fieldWidth: Signal[Int],
      val isRight: Boolean,
      val fieldHeight: Signal[Int],
      val inputY: Signal[Int]
  ) {
    val height = Var(100)
    val width  = Var(10)

    val posX = fieldWidth.map(w => (if isRight then 1 else -1) * (w / 2 - 25))
    val posY = {
      Signal {
        math.max(
          math.min(
            inputY.value,
            (fieldHeight.value - height.value) / 2
          ),
          -(fieldHeight.value - height.value) / 2
        )
      }
    }

    def collisionWith(collider: Shape): Event[Any] = {
      val collisionBoxHeight = Signal { height.value + collider.hitboxHeight.value }
      val collisionBoxWidth  = Signal { width.value + collider.hitboxWidth.value }
      val shapeInsideRacket = Signal {
        (posX.value - collisionBoxWidth.value / 2 < collider.centerX.value) &&
        (posX.value + collisionBoxWidth.value / 2 > collider.centerX.value) &&
        (posY.value - collisionBoxHeight.value / 2 < collider.centerY.value) &&
        (posY.value + collisionBoxHeight.value / 2 > collider.centerY.value)
      }
      shapeInsideRacket.changed.filter(_ == true)
    }

    val shape = new Rectangle(posX, posY, width, height, fill = Var(Some(if isRight then Color.BLUE else Color.RED)))
  }

  val shapes = Var[List[Shape]](List.empty)
  val panel  = new ShapesPanel(shapes)

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
}
