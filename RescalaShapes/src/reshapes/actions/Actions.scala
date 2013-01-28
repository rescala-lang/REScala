package reshapes.actions
import scala.swing.Action
import scala.swing.FileChooser
import java.io.FileOutputStream
import scala.util.Marshal

class SaveAction extends Action("Save") {
  def apply() = {
    val fileChooser = new FileChooser()
    if (fileChooser.showDialog(null, "save") == FileChooser.Result.Approve) {
      val out = new FileOutputStream(fileChooser.selectedFile)
      //out.write(Marshal.dump(events.allShapes.getValue))
      out.close()
    }
  }
}