package ex201x.programming2016demo

import ex201x.programming2016demo.GModularClockCircle.Clock
import ex201x.programming2016demo.ui.{Circle, Point, Shape, ShapesPanel}
import reactives.default.*

/** Eventually, our Pong game will require a bouncing ball that moves
  * on a straight, rather than circular, trajectory. We begin making
  * changes towards this end. We prototype bouncing horizontally and
  * vertically by using left and right mouse clicks respectively.
  * We remove the angle and redefine velocityX and velocityY as folds.
  * Rather than approximately continuous numerical integration, these
  * folds are more discrete in their behavior. Given some initial
  * velocity vector, when a left or right mouse click occurs, the
  * velocity in the respective direction is simply reversed. While
  * no mouse clicks occur, both velocities simply stay constant,
  * i.e., the ball moves in a straight line at constant speed.
  */
object JMouseBouncingBall extends Main {
  val shapes = Var[List[Shape]](List.empty)
  val panel  = new ShapesPanel(shapes)

  val velocity = Signal {
    Pos(
      x = panel.Mouse.leftButton.pressed.fold(200d / Clock.NanoSecond) { (old, _) => -old }.value,
      y = panel.Mouse.rightButton.pressed.fold(150d / Clock.NanoSecond) { (old, _) => -old }.value
    )
  }

  val inc = Clock.ticks.map(tick => Right[Point, Pos](velocity.value * tick.toDouble))

  val reset = panel.Mouse.middleButton.pressed.map(pos => Left[Point, Pos](pos))

  val pos = (reset || inc).fold(Pos(0, 0)) {
    case (_, Left(Point(x, y))) => Pos(x.toDouble, y.toDouble)
    case (pX, Right(inc))       => pX + inc
  }

  shapes.transform(new Circle(pos, Var(50)) :: _)
}
