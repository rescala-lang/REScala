package reshapes.command
import reshapes.Events
import reshapes.figures.Drawable
import java.awt.Point
import reshapes.Drawing
import reshapes.figures.Line
import reshapes.Reshapes

abstract class Command {

  def execute() = {
    onExecute()
    Reshapes.CurrentEvents.Commands() = this :: Reshapes.CurrentEvents.Commands.getValue
  }

  def revert(): Unit = {
    // check if this command is latest command
    while (Reshapes.CurrentEvents.Commands.getValue.first != this) {
      Reshapes.CurrentEvents.Commands.getValue.first.revert()
    }
    onRevert()
    Reshapes.CurrentEvents.Commands() = Reshapes.CurrentEvents.Commands.getValue.tail
  }

  def onExecute()
  def onRevert()
  def getCommandDescription(): String = {
    "Abstract command"
  }
}

/**
 * Deletes a given shape
 */
class DeleteShape(shapeToDelete: Drawable) extends Command {

  def onExecute() = {
    Reshapes.CurrentEvents.allShapes() = Reshapes.CurrentEvents.allShapes.getValue filter (x => x != shapeToDelete)
  }

  def onRevert() = {
    Reshapes.CurrentEvents.allShapes() = shapeToDelete :: Reshapes.CurrentEvents.allShapes.getValue
  }

  override def getCommandDescription(): String = {
    "Delete %s".format(shapeToDelete)
  }
}

class CreateShape(shapeToCreate: Drawable) extends Command {

  def onExecute() {
    Reshapes.CurrentEvents.allShapes() = shapeToCreate :: Reshapes.CurrentEvents.allShapes.getValue
  }

  def onRevert() {
    var deleteCmd = new DeleteShape(shapeToCreate)
    deleteCmd.onExecute()
  }

  override def getCommandDescription(): String = {
    "Create %s".format(shapeToCreate)
  }
}

class EditShape(shapeBeforeEdit: Drawable, shapeAfterEdit: Drawable) extends Command {

  def onExecute() {

  }

  def onRevert() {
    shapeAfterEdit.start = shapeBeforeEdit.start
    shapeAfterEdit.end = shapeBeforeEdit.end
    shapeAfterEdit.strokeWidth = shapeBeforeEdit.strokeWidth
    shapeAfterEdit.color = shapeBeforeEdit.color

    Reshapes.CurrentEvents.selectedShape() = new Line() // XXX: hack to force selectedShape.changed event when Events.selectedShape() == shapeAfterEdit
    Reshapes.CurrentEvents.selectedShape() = shapeAfterEdit
  }

  override def getCommandDescription(): String = {
    "Edit %s".format(shapeAfterEdit)
  }
}