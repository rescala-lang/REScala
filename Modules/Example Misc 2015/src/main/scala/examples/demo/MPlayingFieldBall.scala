package examples.demo

import examples.demo.LFullyModularBall.BouncingBall
import examples.demo.ui.{Rectangle, Shape, ShapesPanel}
import reactives.default.*

/** We now add a playing field to our game. We begin implementing this
  * as a separate module from the beginning. It implements a rectangle
  * that resizes to match the ShapesPanel's size. We add a method to
  * this class, that given a shape (e.g., the ball) returns a set of
  * four collision events, one for each side of the rectangle.
  *
  * To compute these collision events, we first define for each side
  * of the PlayingField a Signal that reflects, whether or not the
  * ball is in bounds w.r.t. this side. We then derive a changed
  * Event, which emits each new value of the signal. Finally, we
  * derive another filtered event, that only forwards the emitted
  * value if it is true, i.e., emits only if the ball moved out
  * of bounds, but does not emit if it moved back in bounds.
  * Alternatively, we can shortcut these two derivations by using
  * changedTo(true), which yields the same result.
  *
  * With all collision events defined, we can now instantiate them
  * for our BouncingBall. Then, we replace the left and right mouse
  * buttons for horizontal and vertical bounces by the respective
  * collision events.
  */
object MPlayingFieldBall extends Main {
  class PlayingField(val width: Signal[Int], val height: Signal[Int]) {
    case class Collisions(left: Event[Any], right: Event[Any], top: Event[Any], bottom: Event[Any])
    def colliders(shape: Shape): Collisions = {
      val horizontalHalfDistance = Signal { (width.value - shape.hitboxWidth.value) / 2 }
      val verticalHalfDistance   = Signal { (height.value - shape.hitboxHeight.value) / 2 }

      Collisions(
        Signal { shape.centerX.value < -horizontalHalfDistance.value }.changed.filter(_ == true),
        Signal { shape.centerX.value > horizontalHalfDistance.value }.changed.filter(_ == true),
        Signal { shape.centerY.value < -verticalHalfDistance.value }.changed.filter(_ == true),
        Signal { shape.centerY.value > verticalHalfDistance.value }.changed.filter(_ == true)
      )
    }
    val shape = new Rectangle(Var(0), Var(0), width, height)
  }

  val shapes = Var[List[Shape]](List.empty)
  val panel  = new ShapesPanel(shapes)

  val playingField = new PlayingField(panel.width.map(_ - 25), panel.height.map(_ - 25))
  shapes.transform(playingField.shape :: _)

  val bouncingBall = new BouncingBall(200d, 150d, Var(50), panel.Mouse.middleButton.pressed)
  shapes.transform(bouncingBall.shape :: _)

  val fieldCollisions = playingField.colliders(bouncingBall.shape)
  bouncingBall.horizontalBounceSources.transform(fieldCollisions.left :: fieldCollisions.right :: _)
  bouncingBall.verticalBounceSources.transform(fieldCollisions.top :: fieldCollisions.bottom :: _)
}
