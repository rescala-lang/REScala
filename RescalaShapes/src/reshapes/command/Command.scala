package reshapes.command
import reshapes.EventHolder
import reshapes.figures.Drawable

trait Command {

  def execute(event: EventHolder, shape: Drawable)
}

class DeleteCommand extends Command {

  def execute(event: EventHolder, shapeToDelete: Drawable) = {
    event.allShapes() = event.allShapes.getValue filter (x => x != shapeToDelete)
  }
}