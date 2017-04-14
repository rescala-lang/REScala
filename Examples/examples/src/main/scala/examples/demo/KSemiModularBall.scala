package examples.demo

import examples.demo.GModularClockCircle.Clock
import examples.demo.ui._
import rescala._

/**
  * We now convert our BouncingBall into an external Module, similar
  * to Clock earlier. We begin by wrapping the movement code into
  * a class. We leave the ball diameter and position reset Event as
  * a Signal and Event that are passed in at instantiation. We make
  * the circle shape a member of the class, to serve as it's output.
  * Finally, to decouple the BouncingBall class entirely from the
  * ShapesPanel, we need to make the horizontal and vertical bounce
  * events a parameter defined from outside as well. But, here we
  * will run into another cyclic dependency issue. Eventually, the
  * ball should bounce when its position collides with the playing
  * field borders, meaning the bounce events that the BouncingBall
  * class requires for instantiation can only be defined based on
  * its output, i.e., after the ball has already been instantiated.
  * This is not a cyclic dependency issue in the sense that it
  * creates an infinite loop in the application, because posX and
  * posY are already defined on velocityX.before and velocityY.before
  * respectively, but it still is a cyclic problem in terms of the
  * instantiation order of the involved Signals and Events.
  */
object KSemiModularBall extends Main {
  val shapes = Var[List[Shape]](List.empty)
  val panel = new ShapesPanel(shapes)

  class BouncingBall(val initVx: Double, val initVy: Double, val diameter: Signal[Int], val reset: Event[Point]) {
    val velocityX = panel.Mouse.leftButton.pressed.fold(initVx / Clock.NanoSecond) { (old, _) => -old }
    val velocityY = panel.Mouse.rightButton.pressed.fold(initVy / Clock.NanoSecond) { (old, _ ) => -old }

    val resetOrTick = Event {Some((reset(), Clock.ticks()))}

    val posX = resetOrTick.fold(0d){
      case (_, (Some(Point(x, _)), _)) => x.toDouble
      case (pX, (None, Some(tick))) => pX + tick.toDouble * velocityX.before
    }
    val posY = resetOrTick.fold(0d){
      case (_, (Some(Point(_, y)), _)) => y.toDouble
      case (pY, (None, Some(tick))) => pY + tick.toDouble * velocityY.before
    }

    val shape = new Circle(posX.map(_.toInt), posY.map(_.toInt), diameter)
  }

  val bouncingBall = new BouncingBall(200d, 150d, Var(50), panel.Mouse.middleButton.pressed)
  shapes.transform(bouncingBall.shape :: _)
}
