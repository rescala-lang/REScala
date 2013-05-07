package reshapes.command
import reshapes.DrawingSpaceState
import reshapes.figures.Shape
import java.awt.Point
import reshapes.Drawing
import reshapes.figures.Line
import reshapes.Reshapes
import java.util.UUID

@serializable
abstract class Command() {

  def execute() = {
    onExecute()
    Reshapes.currentEvents.commands = this :: Reshapes.currentEvents.commands
  }

  def revert(): Unit = {
    // check if this command is latest command
    while (Reshapes.currentEvents.commands.head != this) {
      // if not then revert all commands which where executed after this
      Reshapes.currentEvents.commands.head.revert()
    }
    onRevert()
    Reshapes.currentEvents.commands = Reshapes.currentEvents.commands.tail
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
class DeleteShape(shapeToDelete: Shape) extends Command {

  def onExecute() = {
    Reshapes.currentEvents.allShapes = Reshapes.currentEvents.allShapes filter (x => x != shapeToDelete)
  }

  def onRevert() = {
    Reshapes.currentEvents.allShapes = shapeToDelete :: Reshapes.currentEvents.allShapes
  }

  override def getCommandDescription(): String = {
    "Delete %s".format(shapeToDelete)
  }
}

/**
 * Creates a new shape.
 */
class CreateShape(shapeToCreate: Shape) extends Command {

  def onExecute() {
    Reshapes.currentEvents.allShapes = shapeToCreate :: Reshapes.currentEvents.allShapes
  }

  def onRevert() {
    var deleteCmd = new DeleteShape(shapeToCreate)
    deleteCmd.onExecute()
  }

  override def getCommandDescription(): String = {
    "Create %s".format(shapeToCreate)
  }
}

/**
 * Only implements onRevert() which restores a shape to state before resize, move, ...
 */
class EditShape(shapeBeforeEdit: Shape, shapeAfterEdit: Shape) extends Command {

  def onExecute() {

  }

  def onRevert() {
    shapeAfterEdit.path = shapeBeforeEdit.path
    shapeAfterEdit.strokeWidth = shapeBeforeEdit.strokeWidth
    shapeAfterEdit.color = shapeBeforeEdit.color

    Reshapes.currentEvents.selectedShape = new Line() // force to fire selectedShape.changed event when Events.selectedShape() == shapeAfterEdit
    Reshapes.currentEvents.selectedShape = shapeAfterEdit
  }

  override def getCommandDescription(): String = {
    "Edit %s".format(shapeAfterEdit)
  }
}

/**
 * Adds all shapes of given Events with currently selected Events.
 */
class MergeEvents(eventToMerge: DrawingSpaceState) extends Command {

  var eventTitle: String = null
  var shapes: List[Shape] = null

  def onExecute() {
    eventTitle = eventToMerge.fileName
    shapes = eventToMerge.allShapes

    shapes map (shape => new CreateShape(shape).onExecute())
  }

  def onRevert() {
    shapes map (shape => new DeleteShape(shape).onExecute())
  }

  override def getCommandDescription(): String = {
    "Merge with %s".format(eventTitle)
  }
}

