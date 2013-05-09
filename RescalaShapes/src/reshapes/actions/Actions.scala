package reshapes.actions

import scala.swing.Action
import scala.swing.FileChooser
import java.io.FileOutputStream
import scala.util.Marshal
import reshapes.drawing.DrawingSpaceState
import java.io.FileInputStream
import reshapes.figures.Shape
import reshapes.drawing.CreateShape
import reshapes.Reshapes
import java.io.File
import reshapes.drawing.MergeEvents

/**
 * Serializes all currently drawn shapes to a chosen file.
 */
class SaveAction extends Action("Save") {
  def apply() = {
    val fileChooser = new FileChooser()
    fileChooser.selectedFile = new File(Reshapes.currentEvents.fileName)
    if (fileChooser.showDialog(null, "save") == FileChooser.Result.Approve) {
      val out = new FileOutputStream(fileChooser.selectedFile)
      // object Marshal in package util is deprecated
      out.write(Marshal.dump(Reshapes.currentEvents.allShapes))
      out.close()
      Reshapes.currentEvents.fileName = fileChooser.selectedFile.getName()
      Reshapes.tabbedPane.pages(Reshapes.tabbedPane.selection.index).title = fileChooser.selectedFile.getName() // XXX
    }
  }
}

/**
 * Deserializes shapes from a chosen file.
 */
class LoadAction extends Action("Load") {
  def apply() = {
    val fileChooser = new FileChooser()
    if (fileChooser.showDialog(null, "load") == FileChooser.Result.Approve) {
      val in = new FileInputStream(fileChooser.selectedFile)
      val bytes = Stream.continually(in.read).takeWhile(-1 !=).map(_.toByte).toArray
      val shapes = Marshal.load[List[Shape]](bytes)
      Reshapes.currentEvents.clear
      shapes map (shape => Reshapes.currentEvents execute new CreateShape(shape))
    }
  }
}

/**
 * Closes the application.
 */
class QuitAction extends Action("Quit") {
  def apply() = {
    System.exit(0)
  }
}

/**
 * Reverts last command.
 */
class UndoAction extends Action("Undo") {
  def apply() = {
    Reshapes.currentEvents revert Reshapes.currentEvents.commands.head
  }
}

/**
 * Merges current drawing panel with another panel.
 */
class MergeAction(title: String, eventsToMergeWith: DrawingSpaceState) extends Action("Merge with %s".format(title)) {
  def apply() = {
    Reshapes.currentEvents execute new MergeEvents(eventsToMergeWith)
  }
}