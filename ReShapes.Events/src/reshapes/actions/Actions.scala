package reshapes.actions

import java.io.File

import scala.swing.Action
import scala.swing.FileChooser
import scala.swing.SwingApplication
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
    fileChooser.selectedFile = new File(ReShapes.drawingSpaceState.fileName)
    if (fileChooser.showDialog(null, "save") == FileChooser.Result.Approve) {
      XML.save(fileChooser.selectedFile.getCanonicalPath,
               Shape.serialize(ReShapes.drawingSpaceState.shapes))
      ReShapes.drawingSpaceState.fileName = fileChooser.selectedFile.getName
      ReShapes.ui.tabbedPane.pages(ReShapes.ui.tabbedPane.selection.index).title = fileChooser.selectedFile.getName
    }
  }
}

/**
 * Deserializes shapes from a chosen file
 */
class LoadAction extends Action("Load") {
  def apply() = {
    val fileChooser = new FileChooser
    if (fileChooser.showDialog(null, "load") == FileChooser.Result.Approve) {
      ReShapes.drawingSpaceState.clear
      for (shape <- Shape.deserialize(XML.loadFile(fileChooser.selectedFile),
                                      ReShapes.drawingSpaceState))
        ReShapes.drawingSpaceState execute new CreateShape(shape)
    }
  }
}

/**
 * Closes the application
 */
class QuitAction(app: SwingApplication) extends Action("Quit") {
  def apply() = app.quit
}

/**
 * Reverts last command
 */
class UndoAction extends Action("Undo") {
  def apply() =
    ReShapes.drawingSpaceState revert ReShapes.drawingSpaceState.commands.head
}

/**
 * Merges current drawing panel with another panel
 */
class MergeAction(title: String, eventsToMergeWith: DrawingSpaceState) extends Action("Merge with %s".format(title)) {
  def apply() =
    ReShapes.drawingSpaceState execute new MergeDrawingSpaces(eventsToMergeWith)
}