package reswingexamples.reshapes.actions

import reactives.default.*
import reswingexamples.reshapes.ReShapes
import reswingexamples.reshapes.drawing.CreateShape
import reswingexamples.reshapes.figures.Shape

import java.io.File
import scala.swing.{Action, FileChooser}
import scala.xml.XML

/** Serializes all currently drawn shapes to a chosen file */
class SaveAction extends Action("Save") {
  def apply() = {
    val fileChooser = new FileChooser()
    fileChooser.selectedFile = new File(ReShapes.drawingSpaceState.now.fileName.now)
    if fileChooser.showDialog(null, "save") == FileChooser.Result.Approve then {
      XML.save(fileChooser.selectedFile.getCanonicalPath, Shape.serialize(ReShapes.drawingSpaceState.now.shapes.now))
      ReShapes.drawingSpaceState.now.fileName `set` fileChooser.selectedFile.getName
      ReShapes.ui.tabbedPane.pages(ReShapes.ui.tabbedPane.selection.index).title = fileChooser.selectedFile.getName
    }
  }
}

/** Deserializes shapes from a chosen file */
class LoadAction extends Action("Load") {
  def apply() = {
    val fileChooser = new FileChooser()
    if fileChooser.showDialog(null, "load") == FileChooser.Result.Approve then {
      ReShapes.drawingSpaceState.now.clear.fire()
      for shape <- Shape.deserialize(XML.loadFile(fileChooser.selectedFile), ReShapes.drawingSpaceState.now) do
        ReShapes.drawingSpaceState.now.execute.fire(new CreateShape(shape))
    }
  }
}
