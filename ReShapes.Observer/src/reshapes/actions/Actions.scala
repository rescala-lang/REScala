package reshapes.actions

import java.io.File

import scala.swing.Action
import scala.swing.FileChooser
import scala.xml.XML

import reshapes.Reshapes
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
    fileChooser.selectedFile = new File(Reshapes.drawingSpaceState.fileName)
    if (fileChooser.showDialog(null, "save") == FileChooser.Result.Approve) {
      XML.save(fileChooser.selectedFile.getCanonicalPath,
               Shape.serialize(Reshapes.drawingSpaceState.shapes))
      Reshapes.drawingSpaceState.fileName = fileChooser.selectedFile.getName()
      Reshapes.ui.tabbedPane.pages(Reshapes.ui.tabbedPane.selection.index).title = fileChooser.selectedFile.getName()
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
      Reshapes.drawingSpaceState.clear
      Shape.deserialize(XML.loadFile(fileChooser.selectedFile),
                        Reshapes.drawingSpaceState) map (shape =>
        Reshapes.drawingSpaceState execute new CreateShape(shape))
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
    Reshapes.drawingSpaceState revert Reshapes.drawingSpaceState.commands.head
  }
}

/**
 * Merges current drawing panel with another panel
 */
class MergeAction(title: String, eventsToMergeWith: DrawingSpaceState) extends Action("Merge with %s".format(title)) {
  def apply() = {
    Reshapes.drawingSpaceState execute new MergeDrawingSpaces(eventsToMergeWith)
  }
}