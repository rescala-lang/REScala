package examples.demo

import examples.demo.GModularClockCircle.Clock
import examples.demo.ui.{Circle, Point, Shape, ShapesPanel}
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

  class BouncingBall(val initVx: Double, val initVy: Double, val diameter: Signal[Int], val resetIn: Event[Point]) {
    val velocityX = panel.Mouse.leftButton.pressed.fold(initVx / Clock.NanoSecond) { (old, _) => -old }
    val velocityY = panel.Mouse.rightButton.pressed.fold(initVy / Clock.NanoSecond) { (old, _ ) => -old }

    val incX = Clock.ticks.dMap(dt => tick => Right[Point, Double](tick.toDouble * dt.before(velocityX)))
    val incY = Clock.ticks.dMap(dt => tick => Right[Point, Double](tick.toDouble * dt.before(velocityY)))

    val reset = resetIn.map(pos => Left[Point, Double](pos))

    val posX = (reset || incX).fold(0d){
      case (_, Left(Point(x, _))) => x.toDouble
      case (pX, Right(inc)) => pX + inc
    }
    val posY = (reset || incX).fold(0d){
      case (_, Left(Point(_, y))) => y.toDouble
      case (pY, Right(inc)) => pY + inc
    }

    val shape = new Circle(posX.map(_.toInt), posY.map(_.toInt), diameter)
  }

  val bouncingBall = new BouncingBall(200d, 150d, Var(50), panel.Mouse.middleButton.pressed)
  shapes.transform(bouncingBall.shape :: _)
}
