package examples.demo

import examples.demo.GModularClockCircle.Clock
import examples.demo.ui._
import rescala._

/**
  * Eventually, our Pong game will require a bouncing ball that moves
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
  val panel = new ShapesPanel(shapes)

  val velocityX = panel.Mouse.leftButton.pressed.fold(200d / Clock.NanoSecond) { (old, _) => -old }
  val velocityY = panel.Mouse.rightButton.pressed.fold(150d / Clock.NanoSecond) { (old, _ ) => -old }

  val resetOrTick = Event {Some((panel.Mouse.middleButton.pressed(), Clock.ticks()))}

  val posX = resetOrTick.fold(0d){
    case (_, (Some(Point(x, _)), _)) => x.toDouble
    case (pX, (None, Some(tick))) => pX + tick.toDouble * velocityX.before
  }
  val posY = resetOrTick.fold(0d){
    case (_, (Some(Point(_, y)), _)) => y.toDouble
    case (pY, (None, Some(tick))) => pY + tick.toDouble * velocityY.before
  }

  shapes.transform(new Circle(posX.map(_.toInt), posY.map(_.toInt), Var(50)) :: _)
}
