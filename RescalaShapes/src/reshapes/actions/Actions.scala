package reshapes.actions

import java.awt.Color
import java.awt.Point
import java.io.File

import scala.swing.Action
import scala.swing.FileChooser
import scala.xml.Attribute
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Null
import scala.xml.Text
import scala.xml.XML

import reshapes.Reshapes
import reshapes.drawing.CreateShape
import reshapes.drawing.DrawingSpaceState
import reshapes.drawing.MergeEvents
import reshapes.figures.Freedraw
import reshapes.figures.Line
import reshapes.figures.Oval
import reshapes.figures.Rectangle
import reshapes.figures.Shape
import reshapes.figures.Triangle

/**
 * Serializes all currently drawn shapes to a chosen file
 */
class SaveAction extends Action("Save") {
  def apply() = {
    val fileChooser = new FileChooser()
    fileChooser.selectedFile = new File(Reshapes.currentEvents.fileName)
    if (fileChooser.showDialog(null, "save") == FileChooser.Result.Approve) {
      def pathToXml(shape: Shape) =
        shape.path map { p => <point x={ p.x.toString } y={ p.y.toString } /> }
      
      def shapeProps(shape: Shape, elem: Elem) =
        elem %
          Attribute(None, "stroke-width", Text(shape.strokeWidth.toString), Null) %
          Attribute(None, "color", Text(shape.color.getRGB.toString), Null) %
          Attribute(None, "current", Text(shape.current.toString), Null)
          
      
      val data = <shapes> {
        Reshapes.currentEvents.allShapes map {
          case shape: Freedraw => shapeProps(shape, <freedraw> { pathToXml(shape) } </freedraw>)
          case shape: Line => shapeProps(shape, <line> { pathToXml(shape) } </line>)
          case shape: Oval => shapeProps(shape, <oval> { pathToXml(shape) } </oval>)
          case shape: Rectangle => shapeProps(shape, <rectangle> { pathToXml(shape) } </rectangle>)
          case shape: Triangle => shapeProps(shape, <triangle> { pathToXml(shape) } </triangle>)
          case shape => throw new UnsupportedOperationException("Saving type " + shape.getClass.getSimpleName + " not implemented.")
        }
      }  </shapes>
      
      XML.save(fileChooser.selectedFile.getCanonicalPath, data)
      
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
      val data = XML.load(fileChooser.selectedFile.getCanonicalPath)
      
      def shapeProps(elem: Node) =
        (elem \ "point" map { p =>
          new Point((p attribute "x").get.text.toInt, (p attribute "y").get.text.toInt)
        }).toList
      
      val shapes = if (data.label == "shapes")
        data.child collect {
          case shape if shape.label == "freedraw" =>
            new Freedraw(Reshapes.currentEvents,
                path = shapeProps(shape),
                strokeWidth = (shape attribute "stroke-width").get.text.toInt,
                color = Color.decode((shape attribute "color").get.text),
                current = (shape attribute "current").get.text.toInt)
          case shape if shape.label == "line" =>
            new Line(Reshapes.currentEvents,
                path = shapeProps(shape),
                strokeWidth = (shape attribute "stroke-width").get.text.toInt,
                color = Color.decode((shape attribute "color").get.text),
                current = (shape attribute "current").get.text.toInt)
          case shape if shape.label == "oval" =>
            new Oval(Reshapes.currentEvents,
                path = shapeProps(shape),
                strokeWidth = (shape attribute "stroke-width").get.text.toInt,
                color = Color.decode((shape attribute "color").get.text),
                current = (shape attribute "current").get.text.toInt)
          case shape if shape.label == "rectangle" =>
            new Rectangle(Reshapes.currentEvents,
                path = shapeProps(shape),
                strokeWidth = (shape attribute "stroke-width").get.text.toInt,
                color = Color.decode((shape attribute "color").get.text),
                current = (shape attribute "current").get.text.toInt)
          case shape if shape.label == "triangle" =>
            new Triangle(Reshapes.currentEvents,
                path = shapeProps(shape),
                strokeWidth = (shape attribute "stroke-width").get.text.toInt,
                color = Color.decode((shape attribute "color").get.text),
                current = (shape attribute "current").get.text.toInt)
        }
      else
        List.empty
      
      Reshapes.currentEvents.clear
      shapes map (shape => Reshapes.currentEvents execute new CreateShape(shape))
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