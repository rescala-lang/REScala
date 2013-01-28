package reshapes.command
import reshapes.Events
import reshapes.figures.Drawable
import java.awt.Point
import reshapes.Drawing
import reshapes.figures.Line

abstract class Command {

  def execute() = {
    onExecute()
    Events.Commands() = this :: Events.Commands.getValue
  }

  def revert() = {
    onRevert()
    Events.Commands() = Events.Commands.getValue.tail
  }

  def onExecute()
  def onRevert()
}

/**
 * Deletes a given shape
 */
class DeleteCommand(shapeToDelete: Drawable) extends Command {

  def onExecute() = {
    Events.allShapes() = Events.allShapes.getValue filter (x => x != shapeToDelete)
  }

  def onRevert() = {
    Events.allShapes() = shapeToDelete :: Events.allShapes.getValue
  }
}

class CreateShapeCommand(shapeToCreate: Drawable) extends Command {

  def onExecute() {
    Events.allShapes() = shapeToCreate :: Events.allShapes.getValue
  }

  def onRevert() {
    var deleteCmd = new DeleteCommand(shapeToCreate)
    deleteCmd.onExecute()
  }
}

class EditShapeCommand(shapeBeforeEdit: Drawable, shapeAfterEdit: Drawable) extends Command {

  def onExecute() {

  }

  def onRevert() {
    shapeAfterEdit.start = shapeBeforeEdit.start
    shapeAfterEdit.end = shapeBeforeEdit.end
    shapeAfterEdit.strokeWidth = shapeBeforeEdit.strokeWidth
    shapeAfterEdit.color = shapeBeforeEdit.color

    Events.selectedShape() = new Line() // XXX: hack to force selectedShape.changed event when Events.selectedShape() == shapeAfterEdit
    Events.selectedShape() = shapeAfterEdit
  }
}