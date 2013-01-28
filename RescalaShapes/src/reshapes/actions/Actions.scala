package reshapes.actions
import scala.swing.Action
import scala.swing.FileChooser
import java.io.FileOutputStream
import scala.util.Marshal
import reshapes.Events
import java.io.FileInputStream
import reshapes.figures.Drawable
import reshapes.command.CreateShapeCommand

/**
 * Serializes all currently drawn shapes to a chosen file.
 */
class SaveAction extends Action("Save") {
  def apply() = {
    val fileChooser = new FileChooser()
    if (fileChooser.showDialog(null, "save") == FileChooser.Result.Approve) {
      val out = new FileOutputStream(fileChooser.selectedFile)
      out.write(Marshal.dump(Events.allShapes.getValue))
      out.close()
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
      val shapes = Marshal.load[List[Drawable]](bytes)
      Events.allShapes() = List[Drawable]()
      shapes map (shape => (new CreateShapeCommand(shape)).execute())
    }
  }
}

class QuitAction extends Action("Quit") {
  def apply() = {
    System.exit(0)
  }
}

class UndoAction extends Action("Undo") {
  def apply() = {
    Events.Commands.getValue.first.revert()
  }
}