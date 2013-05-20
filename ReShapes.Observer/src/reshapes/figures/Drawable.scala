package reshapes.figures

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Point

import scala.xml.Attribute
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Null
import scala.xml.Text

import reshapes.drawing.DrawingSpaceState
import reshapes.util.MathUtil

abstract class Shape(
    val drawingSpaceState: DrawingSpaceState,
    val strokeWidth: Int = 1,
    val color: Color = Color.BLACK,
    val current: Int = 0,
    val path: List[Point] = List.empty /* the mouse path while drawing this shape */) {
  
  def selected = drawingSpaceState.selectedShape == this
  def start = if (path.isEmpty) null else path.head
  def end = if (path.isEmpty) null else path.last
  
  def draw(g: Graphics2D) = {
    if (start != null && end != null) {
      val stroke = if (!selected) new BasicStroke(strokeWidth) else new BasicStroke(strokeWidth,
        BasicStroke.CAP_BUTT,
        BasicStroke.JOIN_MITER,
        10.0f, Array(10.0f), 0.0f)
      
      g.setStroke(stroke)
      g.setColor(color)
      doDraw(g)
    }
  }
  
  def copy(
      drawingSpaceState: DrawingSpaceState = drawingSpaceState,
      strokeWidth: Int = strokeWidth,
      color: Color = color,
      current: Int = current,
      path: List[Point] = path): Shape
  
  override def toString(): String =
    this.getClass.getSimpleName + " #" + current.toString
  
  def doUpdate(path: List[Point]) = {}
  def doDraw(g: Graphics2D)
  
  def toLines(): List[(Point, Point)]
}

object Shape {
  var current = 0
  
  def serialize(shapes: List[Shape]): Elem = {
    def shapePath(shape: Shape) =
      shape.path map { p => <point x={ p.x.toString } y={ p.y.toString } /> }
      
      def shapeProps(shape: Shape, elem: Elem) =
        elem %
          Attribute(None, "stroke-width", Text(shape.strokeWidth.toString), Null) %
          Attribute(None, "color", Text(shape.color.getRGB.toString), Null) %
          Attribute(None, "current", Text(shape.current.toString), Null)
        
    
    <shapes> {
      shapes map {
        case shape: Freedraw => shapeProps(shape, <freedraw> { shapePath(shape) } </freedraw>)
        case shape: Line => shapeProps(shape, <line> { shapePath(shape) } </line>)
        case shape: Oval => shapeProps(shape, <oval> { shapePath(shape) } </oval>)
        case shape: Rectangle => shapeProps(shape, <rectangle> { shapePath(shape) } </rectangle>)
        case shape: Triangle => shapeProps(shape, <triangle> { shapePath(shape) } </triangle>)
        case shape => throw new UnsupportedOperationException("Saving type " + shape.getClass.getSimpleName + " not implemented.")
      }
    }  </shapes>
  }
  
  def deserialize(data: Elem, drawingSpaceState: DrawingSpaceState): List[Shape] = {
    def shapePath(elem: Node) =
      (elem \ "point" map { p =>
        new Point((p attribute "x").get.text.toInt, (p attribute "y").get.text.toInt)
      }).toList
    
    if (data.label == "shapes")
      (data.child collect {
        case shape if shape.label == "freedraw" =>
          new Freedraw(drawingSpaceState,
              path = shapePath(shape),
              strokeWidth = (shape attribute "stroke-width").get.text.toInt,
              color = Color.decode((shape attribute "color").get.text),
              current = (shape attribute "current").get.text.toInt)
        case shape if shape.label == "line" =>
          new Line(drawingSpaceState,
              path = shapePath(shape),
              strokeWidth = (shape attribute "stroke-width").get.text.toInt,
              color = Color.decode((shape attribute "color").get.text),
              current = (shape attribute "current").get.text.toInt)
        case shape if shape.label == "oval" =>
          new Oval(drawingSpaceState,
              path = shapePath(shape),
              strokeWidth = (shape attribute "stroke-width").get.text.toInt,
              color = Color.decode((shape attribute "color").get.text),
              current = (shape attribute "current").get.text.toInt)
        case shape if shape.label == "rectangle" =>
          new Rectangle(drawingSpaceState,
              path = shapePath(shape),
              strokeWidth = (shape attribute "stroke-width").get.text.toInt,
              color = Color.decode((shape attribute "color").get.text),
              current = (shape attribute "current").get.text.toInt)
        case shape if shape.label == "triangle" =>
          new Triangle(drawingSpaceState,
              path = shapePath(shape),
              strokeWidth = (shape attribute "stroke-width").get.text.toInt,
              color = Color.decode((shape attribute "color").get.text),
              current = (shape attribute "current").get.text.toInt)
      }).toList
    else
      List.empty
  }
}

trait Movable extends Shape {
  def movedShape(from: Point, to: Point) =
    copy(path = path map {p => new Point(p.x + to.x - from.x, p.y + to.y - from.y)})
}

trait Resizable extends Shape {
  def resizedShape(from: Point, to: Point) = {
    if (MathUtil.isInCircle(start, 6, from))
      copy(path = to :: path)
    else if (MathUtil.isInCircle(end, 6, from))
      copy(path = path :+ to)
    else
      this: Shape
  }
  
  override def draw(g: Graphics2D) = {
    super.draw(g)
    if (start != null && end != null && selected) {
      val origStroke = g.getStroke
      g.setStroke(new BasicStroke(1))
      g.setColor(new Color(200, 200, 200))
      
      g.drawOval(start.x - 5, start.y - 5, 10, 10)
      g.drawOval(end.x - 5, end.y - 5, 10, 10)
      
      g.setStroke(origStroke)
    }
  }
}
