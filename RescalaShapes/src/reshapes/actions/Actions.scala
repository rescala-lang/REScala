package reshapes.actions

import java.io.File

import scala.swing.Action
import scala.swing.FileChooser
import scala.xml.XML

import reshapes.Reshapes
import reshapes.drawing.CreateShape
import reshapes.drawing.DrawingSpaceState
import reshapes.drawing.MergeEvents
import reshapes.figures.Shape

/**
 * Serializes all currently drawn shapes to a chosen file
 */
class SaveAction extends Action("Save") {
  def apply() = {
    val fileChooser = new FileChooser()
    fileChooser.selectedFile = new File(Reshapes.currentEvents.fileName)
    if (fileChooser.showDialog(null, "save") == FileChooser.Result.Approve) {
      XML.save(fileChooser.selectedFile.getCanonicalPath,
               Shape.serialize(Reshapes.currentEvents.allShapes))
      Reshapes.currentEvents.fileName = fileChooser.selectedFile.getName()
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
      Reshapes.currentEvents.clear
      Shape.deserialize(XML.loadFile(fileChooser.selectedFile),
                        Reshapes.currentEvents) map (shape =>
        Reshapes.currentEvents execute new CreateShape(shape))
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
    Reshapes.currentEvents revert Reshapes.currentEvents.commands.head
  }
}

/**
 * Merges current drawing panel with another panel
 */
class MergeAction(title: String, eventsToMergeWith: DrawingSpaceState) extends Action("Merge with %s".format(title)) {
  def apply() = {
    Reshapes.currentEvents execute new MergeEvents(eventsToMergeWith)
  }
}