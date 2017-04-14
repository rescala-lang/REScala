package examples.demo

import examples.demo.GModularClockCircle.Clock
import examples.demo.ui.{Circle, Point, Shape, ShapesPanel}
import rescala._

/**
  * We now begin to introduce user interaction. Currently, the
  * ball easily moves outside of the window and correcting its
  * trajectory is very difficult. To simplify this, we will make
  * its posX and posY resettable by clicking the middle mouse
  * button. The shapesPanel offers access to a Mouse object.
  * It contains Signals x and y reflecting the cursor's current
  * coordinates, and leftButton/rightButton/middleButton objects
  * each with pressed and released Events of type Point(x,y);
  * middleButton.pressed is the event we want to use for
  * resetting the moving circle's position
  *
  * Both posX and posY of the ball are currently defined as
  * numerical integration, folding of clock ticks. But, now,
  * they need to fold over reset events at the same time.
  * To fold over multiple Events at once, we introduce Event
  * expressions. We already saw Signal expressions, which
  * can access other Signals values and simply return a new
  * value. Event expressions work similarly, except for
  * different type signatures: While a Signal always has
  * a value, an Event may or may not have a value, so all
  * values of Events are wrapped as Options. In our Event
  * expression, we access middleButton.pressed and Clock.ticks
  * and simply return both values as a Tuple of Option[Point]
  * and Option[Tick]. We also need to wrap the Tuple into an
  * Option using Some(...), as the Event expression could also
  * choose to not emit a value by returning None.
  *
  * We now redefine the numeric integration fold of posX and
  * posY to fold over the new, combined Event. We implement
  * it as a patter match: The first case ignores the old
  * position and Clock tick if a reset value is given and
  * simply uses the reset value as new position. The second
  * case, if no reset event is given, simply executes the
  * euler method integration as before. This pattern is
  * easily extensible to implement folding of any number
  * of different Events into a single Signal.
  */
object INumericResettableCircle extends Main {
  val shapes = Var[List[Shape]](List.empty)
  val panel = new ShapesPanel(shapes)

  val angle = Clock.nsTime.map( _.toDouble / Clock.NanoSecond * math.Pi)

  val velocityX = Signal {(panel.width() / 2 - 50).toDouble * math.sin(angle()) / Clock.NanoSecond}
  val velocityY = Signal {(panel.height() / 2 - 50).toDouble * math.cos(angle()) / Clock.NanoSecond}

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
