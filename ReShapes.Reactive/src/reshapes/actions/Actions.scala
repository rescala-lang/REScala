package reshapes.actions

import java.io.File

import scala.swing.Action
import scala.swing.FileChooser
import scala.xml.XML

import reshapes.ReShapes
import reshapes.drawing.CreateShape
import reshapes.drawing.DrawingSpaceState
import reshapes.drawing.MergeDrawingSpaces
import reshapes.figures.Shape

/**
 * Serializes all currently drawn shapes to a chosen file
 */
class SaveAction extends Action("Save") {
  def apply() = {
    val fileChooser = new FileChooser()
    fileChooser.selectedFile = new File(ReShapes.drawingSpaceState.getValue.fileName.getValue)
    if (fileChooser.showDialog(null, "save") == FileChooser.Result.Approve) {
      XML.save(fileChooser.selectedFile.getCanonicalPath,
               Shape.serialize(ReShapes.drawingSpaceState.getValue.shapes.getValue))
      ReShapes.drawingSpaceState.getValue.fileName() = fileChooser.selectedFile.getName
      ReShapes.ui.tabbedPane.pages(ReShapes.ui.tabbedPane.selection.index).title = fileChooser.selectedFile.getName
    }
  }
}

/**
 * Deserializes shapes from a chosen file
 */
class LoadAction extends Action("Load") {
  def apply() = {
    val fileChooser = new FileChooser()
    if (fileChooser.showDialog(null, "load") == FileChooser.Result.Approve) {
      ReShapes.drawingSpaceState.getValue.clear
      for (shape <- Shape.deserialize(XML.loadFile(fileChooser.selectedFile),
                                      ReShapes.drawingSpaceState.getValue))
        ReShapes.drawingSpaceState.getValue execute new CreateShape(shape)
    }
  }
}

/**
 * Closes the application
 */
class QuitAction extends Action("Quit") {
  def apply() = {
    System.exit(0)
  }
}

/**
 * Reverts last command
 */
class UndoAction extends Action("Undo") {
  def apply() = {
    ReShapes.drawingSpaceState.getValue revert ReShapes.drawingSpaceState.getValue.commands.getValue.head
  }
}

/**
 * Merges current drawing panel with another panel
 */
class MergeAction(title: String, eventsToMergeWith: DrawingSpaceState) extends Action("Merge with %s".format(title)) {
  def apply() = {
    ReShapes.drawingSpaceState.getValue execute new MergeDrawingSpaces(eventsToMergeWith)
  }
}