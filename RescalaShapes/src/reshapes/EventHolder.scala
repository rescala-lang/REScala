package reshapes
import scala.events.ImperativeEvent
import reshapes.figures.Drawable
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import reshapes.figures.Line

/**
 * Unifies all events which can occure during execution
 */
class EventHolder {

  val selectedShape: Var[Drawable] = new Var(new Line)
  val strokeWidth: Var[Int] = new Var(1)

  selectedShape.changed += (shape => shape.strokeWidth = strokeWidth.getValue)
}