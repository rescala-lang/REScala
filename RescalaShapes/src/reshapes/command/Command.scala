package reshapes.command
import reshapes.Events
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
class DeleteCommand(shapeToDelete: Drawable) extends Command {

  def execute() = {
    Events.allShapes() = Events.allShapes.getValue filter (x => x != shapeToDelete)
  }

  def revert() = {
    Events.allShapes() = shapeToDelete :: Events.allShapes.getValue
  }
}

class CreateShapeCommand(shapeToCreate: Drawable) extends Command {

  def execute() {
    Events.allShapes() = shapeToCreate :: Events.allShapes.getValue
  }

  def revert() {
    var deleteCmd = new DeleteCommand(shapeToCreate)
    deleteCmd.execute()
  }
}

class EditShapeCommand(shapeBeforeEdit: Drawable, shapeAfterEdit: Drawable) extends Command {

  def execute() {

  }

  def revert() {
    shapeAfterEdit.start = shapeBeforeEdit.start
    shapeAfterEdit.end = shapeBeforeEdit.end
    shapeAfterEdit.strokeWidth = shapeBeforeEdit.strokeWidth
    shapeAfterEdit.color = shapeBeforeEdit.color

    Events.selectedShape() = new Line() // XXX: hack to force selectedShape.changed event when Events.selectedShape() == shapeAfterEdit
    Events.selectedShape() = shapeAfterEdit
  }
}