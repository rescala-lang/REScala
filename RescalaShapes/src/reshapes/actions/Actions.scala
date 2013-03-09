package reshapes.actions
import scala.swing.Action
import scala.swing.FileChooser
import java.io.FileOutputStream
import scala.util.Marshal
import reshapes.Events
import java.io.FileInputStream
import reshapes.figures.Shape
import reshapes.command.CreateShape
import reshapes.Reshapes
import java.io.File
import reshapes.command.MergeEvents

/**
 * Serializes all currently drawn shapes to a chosen file.
 */
class SaveAction extends Action("Save") {
  def apply() = {
    val fileChooser = new FileChooser()
    fileChooser.selectedFile = new File(Reshapes.CurrentEvents.fileName.getValue)
    if (fileChooser.showDialog(null, "save") == FileChooser.Result.Approve) {
      val out = new FileOutputStream(fileChooser.selectedFile)
      out.write(Marshal.dump(Reshapes.CurrentEvents.allShapes.getValue))
      out.close()
      Reshapes.CurrentEvents.fileName() = fileChooser.selectedFile.getName()
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
      Reshapes.CurrentEvents.allShapes() = List[Shape]()
      shapes map (shape => (new CreateShape(shape)).execute())
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
    Reshapes.CurrentEvents.Commands.getValue.first.revert()
  }
}

/**
 * Merges current drawing panel with another panel.
 */
class MergeAction(title: String, eventsToMergeWith: Events) extends Action("Merge with %s".format(title)) {
  def apply() = {
    new MergeEvents(eventsToMergeWith).execute()
  }
}