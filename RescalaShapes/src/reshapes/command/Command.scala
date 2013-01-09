package reshapes.command
import reshapes.EventHolder
import reshapes.figures.Drawable

abstract class Command(events: EventHolder, shape: Drawable) {

  def execute()

  def revert() // revert as trait? (DeleteCommand with Revertable)
}

/**
 * Deletes a given shape
 */
class DeleteCommand(events: EventHolder, shapeToDelete: Drawable) extends Command(events, shapeToDelete) {

  def execute() = {
    events.allShapes() = events.allShapes.getValue filter (x => x != shapeToDelete)
  }

  def revert() = {
    events.allShapes() = shapeToDelete :: events.allShapes.getValue
  }
}