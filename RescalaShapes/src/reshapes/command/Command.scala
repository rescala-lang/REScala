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

  def revert(): Unit = {
    // check if this command is latest command
    while (Events.Commands.getValue.first != this) {
      Events.Commands.getValue.first.revert()
    }
    onRevert()
    Events.Commands() = Events.Commands.getValue.tail
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
    Events.allShapes() = Events.allShapes.getValue filter (x => x != shapeToDelete)
  }

  def onRevert() = {
    Events.allShapes() = shapeToDelete :: Events.allShapes.getValue
  }

  override def getCommandDescription(): String = {
    "Delete %s".format(shapeToDelete)
  }
}

class CreateShape(shapeToCreate: Drawable) extends Command {

  def onExecute() {
    Events.allShapes() = shapeToCreate :: Events.allShapes.getValue
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

    Events.selectedShape() = new Line() // XXX: hack to force selectedShape.changed event when Events.selectedShape() == shapeAfterEdit
    Events.selectedShape() = shapeAfterEdit
  }

  override def getCommandDescription(): String = {
    "Edit %s".format(shapeAfterEdit)
  }
}