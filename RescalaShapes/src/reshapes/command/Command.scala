package reshapes.command
import reshapes.DrawingSpaceState
import reshapes.figures.Shape
import java.awt.Point
import reshapes.Drawing
import reshapes.figures.Line
import reshapes.Reshapes
import java.util.UUID

abstract class Command(val drawingSpaceState: DrawingSpaceState) {
  def execute() =
    drawingSpaceState.commands = this :: drawingSpaceState.commands
  
  def revert() {
    // revert all commands which were executed after this command
    while (drawingSpaceState.commands.head != this)
      drawingSpaceState.commands.head.revert
    drawingSpaceState.commands = drawingSpaceState.commands.tail
  }
  
  def getCommandDescription() = "Abstract command"
}

/**
 * Deletes a given shape
 */
class DeleteShape(drawingSpaceState: DrawingSpaceState, shapeToDelete: Shape)
    extends Command(drawingSpaceState) {
  override def execute() {
    super.execute
    drawingSpaceState removeShape shapeToDelete
  }

  override def revert() {
    super.revert
    drawingSpaceState addShape shapeToDelete
  }
  
  override def getCommandDescription() = "Delete %s".format(shapeToDelete)
}

/**
 * Creates a new shape
 */
class CreateShape(drawingSpaceState: DrawingSpaceState, shapeToCreate: Shape)
    extends Command(drawingSpaceState) {
  override def execute() {
    super.execute
    drawingSpaceState addShape shapeToCreate
  }
  
  override def revert() {
    super.revert
    drawingSpaceState removeShape shapeToCreate
  }
  
  override def getCommandDescription() = "Create %s".format(shapeToCreate)
}

/**
 * Edits a shape, i.e. replaces a shape by a new one
 */
class EditShape(drawingSpaceState: DrawingSpaceState, shapeBeforeEdit: Shape, shapeAfterEdit: Shape)
    extends Command(drawingSpaceState) {
  override def execute() {
    super.execute
    drawingSpaceState removeShape shapeBeforeEdit
    drawingSpaceState addShape shapeAfterEdit
  }

  override def revert() {
    super.revert
    drawingSpaceState removeShape shapeAfterEdit
    drawingSpaceState addShape shapeBeforeEdit
  }
  
  override def getCommandDescription() = "Edit %s".format(shapeAfterEdit)
}

/**
 * Adds all shapes of given Events with currently selected Events.
 */
class MergeEvents(drawingSpaceState: DrawingSpaceState, eventToMerge: DrawingSpaceState)
    extends Command(drawingSpaceState) {
  val eventTitle = eventToMerge.fileName
  val shapes: List[Shape] = eventToMerge.allShapes
  
  override def execute() {
    super.execute
    shapes foreach (drawingSpaceState addShape _)
  }
  
  override def revert() {
    super.revert
    shapes foreach (drawingSpaceState removeShape _)
  }
  
  override def getCommandDescription() = "Merge with %s".format(eventTitle)
}

