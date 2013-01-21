package reshapes.command
import reshapes.EventHolder
import reshapes.figures.Drawable
import java.awt.Point
import reshapes.Drawing
import reshapes.figures.Line

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

class EditShapeCommand(events: EventHolder, shapeBeforeEdit: Drawable, shapeAfterEdit: Drawable) extends Command {

  def execute() {

  }

  def revert() {
    shapeAfterEdit.start = shapeBeforeEdit.start
    shapeAfterEdit.end = shapeBeforeEdit.end
    shapeAfterEdit.strokeWidth = shapeBeforeEdit.strokeWidth
    shapeAfterEdit.color = shapeBeforeEdit.color

    events.selectedShape() = new Line() // XXX: hack to force selectedShape.changed event when events.selectedShape() == shapeAfterEdit
    events.selectedShape() = shapeAfterEdit
  }
}