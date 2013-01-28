package reshapes
import scala.events.ImperativeEvent
import reshapes.figures.Drawable
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import reshapes.figures.Line
import java.awt.Color
import reshapes.command.Command
import scala.events.scalareact

/**
 * Unifies all events which can occure during execution
 */
object Events {

  val nextShape: Var[Drawable] = new Var(new Line)
  val selectedShape: Var[Drawable] = new Var(null)
  val allShapes: Var[List[Drawable]] = new Var(List[Drawable]())
  val strokeWidth: Var[Int] = new Var(1)
  val color: Var[Color] = new Var(Color.BLACK)
  val Commands: Var[List[Command]] = new Var(List[Command]())

  var mode: EditingMode = Drawing()
  val modeChange = nextShape.changed || selectedShape.changed

  val canvasChange = selectedShape.changed || allShapes.changed || modeChange

  nextShape.changed += (shape => {
    shape.strokeWidth = strokeWidth.getValue
    shape.color = color.getValue
    allShapes.getValue map (x => x.selected = false)
    mode = Drawing()
  })

  selectedShape.changed += (shape => {
    allShapes.getValue map (x => x.selected = false)
    shape.selected = true
    mode = Selection()
  })

  val e = new ImperativeEvent[Int]
  //val accum = e.fold(0) { _ + _ }
  val accum = Signal { strokeWidth() }

  val flow2 = scalareact.Signal.flow("No occurence") { self =>
    while (true) {
      self awaitNext accum
      println(accum.getValue)
    }
  }
}

abstract class EditingMode
case class Drawing extends EditingMode
case class Selection extends EditingMode