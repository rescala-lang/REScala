package reshapes.drawing

import reshapes.figures.Shape

abstract class Command {
  def execute(shapes: List[Shape]): List[Shape]
  
  def revert(shapes: List[Shape]): List[Shape]
  
  def description: String
}

/**
 * Deletes a given shape
 */
class DeleteShape(shapeToDelete: Shape) extends Command {
  override def execute(shapes: List[Shape]) =
    shapes filterNot (_ == shapeToDelete)

  override def revert(shapes: List[Shape]) =
    shapeToDelete :: shapes
  
  override def description = "Delete %s".format(shapeToDelete)
}

/**
 * Creates a new shape
 */
class CreateShape(shapeToCreate: Shape) extends Command {
  override def execute(shapes: List[Shape]) =
    shapeToCreate :: shapes

  override def revert(shapes: List[Shape]) =
    shapes filterNot (_ == shapeToCreate)
  
  override def description = "Create %s".format(shapeToCreate)
}

/**
 * Edits a shape, i.e. replaces a shape by a new one
 */
class EditShape(shapeBeforeEdit: Shape, shapeAfterEdit: Shape) extends Command {
  override def execute(shapes: List[Shape]) =
    shapeAfterEdit :: shapes filterNot (_ == shapeBeforeEdit)

  override def revert(shapes: List[Shape]) =
    shapeBeforeEdit :: shapes filterNot (_ == shapeAfterEdit)
  
  override def description = "Edit %s".format(shapeAfterEdit)
}

/**
 * Adds all shapes of given Events with currently selected Events.
 */
class MergeDrawingSpaces(eventToMerge: DrawingSpaceState) extends Command {
  val eventTitle = eventToMerge.fileName
  val otherShapes = eventToMerge.shapes
  
  override def execute(shapes: List[Shape]) =
    otherShapes.get ::: shapes

  override def revert(shapes: List[Shape]) =
    shapes filterNot (otherShapes.get contains _)
  
  override def description = "Merge with %s".format(eventTitle.get)
}

