package reshapes.command
import reshapes.EventHolder
import reshapes.figures.Drawable
import java.awt.Point

abstract class Command {

  def execute()

  def revert() // revert as trait? (DeleteCommand with Revertable)
}

/**
 * Deletes a given shape
 */
class DeleteCommand(events: EventHolder, shapeToDelete: Drawable) extends Command {

  def execute() = {
    events.allShapes() = events.allShapes.getValue filter (x => x != shapeToDelete)
  }

  def revert() = {
    events.allShapes() = shapeToDelete :: events.allShapes.getValue
  }
}

class CreateShapeCommand(events: EventHolder, shapeToCreate: Drawable) extends Command {

  def execute() {
    events.allShapes() = shapeToCreate :: events.allShapes.getValue
  }

  def revert() {
    var deleteCmd = new DeleteCommand(events, shapeToCreate)
    deleteCmd.execute()
  }
}