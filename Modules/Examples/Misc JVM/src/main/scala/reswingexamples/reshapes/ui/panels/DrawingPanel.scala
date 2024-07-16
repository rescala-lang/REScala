package reswingexamples.reshapes.ui.panels

import reactives.default.*
import reswingexamples.reshapes.drawing.{Command, CreateShape, DrawingSpaceState, EditShape}
import reswingexamples.reshapes.figures.{Movable, Resizable, Shape}
import reswingexamples.reshapes.util.MathUtil

import java.awt.BasicStroke
import scala.collection.mutable.ListBuffer
import scala.swing.event.{MouseDragged, MousePressed, MouseReleased}
import scala.swing.{Color, Graphics2D, Panel, Point}

/** Represents the panel where all shapes are drawn onto */
class DrawingPanel(val state: DrawingSpaceState) extends Panel {
  private var point: Point          = null
  private var currentShape: Shape   = null
  private var resizingMode: Boolean = false

  override def paint(g: Graphics2D): Unit = {
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, size.width, size.height)

    g.setColor(java.awt.Color.BLACK)
    if currentShape != null then {
      currentShape.draw(g)
      for shape <- state.shapes.now do
        if !shape.selected then
          shape.draw(g)
    } else
      for shape <- state.shapes.now do
        shape.draw(g)
  }

  lazy val drawn = Evt[Command]() // #EVT

  listenTo(mouse.clicks, mouse.moves)

  reactions += {
    case e: MousePressed =>
      point = e.point
      state.selectedShape.now match {
        case null =>
          currentShape = state.nextShape.now.copy(
            path = List(point),
            strokeWidth = state.strokeWidth.now,
            color = state.color.now,
            current = { Shape.current += 1; Shape.current }
          )
        case selectedShape =>
          currentShape = selectedShape.copy()
          resizingMode = MathUtil.isInCircle(currentShape.start, 6, e.point) ||
            MathUtil.isInCircle(currentShape.end, 6, e.point)
      }
    case e: MouseDragged =>
      state.selectedShape.now match {
        case null =>
          currentShape = currentShape.copy(path = e.point :: currentShape.path)
        case _ =>
          if resizingMode && currentShape.isInstanceOf[Resizable] then
            currentShape = currentShape.asInstanceOf[Resizable].resizedShape(point, e.point)
          else if currentShape.isInstanceOf[Movable] then
            currentShape = currentShape.asInstanceOf[Movable].movedShape(point, e.point)
      }
      point = e.point
      repaint()
    case e: MouseReleased =>
      state.selectedShape.now match {
        case null =>
          drawn.fire(new CreateShape(currentShape))
        case selectedShape =>
          drawn.fire(new EditShape(selectedShape, currentShape))
          state.select.fire(currentShape)
      }
      currentShape = null
      repaint()
  }

  (state.selectedShape.changed ||                      // #IF //#EF
  state.shapes.changed ||                              // #IF //#EF
  state.strokeWidth.changed ||                         // #IF //#EF
  state.color.changed) `observe` (_ => canvasChange()) // #IF //#HDL

  def canvasChange() = repaint()
}

/** This trait draws intersection points between all drawn shapes */
trait ShowIntersection extends DrawingPanel {
  override def paint(g: Graphics2D): Unit = {
    super.paint(g)
    g.setColor(new Color(255, 0, 0))
    g.setStroke(new BasicStroke)
    for point <- getIntersectionPoints() do
      g.drawOval(point.x - 3, point.y - 3, 6, 6)
  }

  def getIntersectionPoints() = {
    val points = new ListBuffer[Point]

    for shape <- state.shapes.now do
      for otherShape <- state.shapes.now do
        if shape != otherShape then
          for line <- shape.toLines() do
            for otherLine <- otherShape.toLines() do {
              val intersection = MathUtil.getIntersectionsOfTwoLines(line, otherLine)
              if intersection != null then
                points += intersection
            }

    points.toList
  }
}

/** Draws a coordinate System onto the panel */
trait ShowCoordinateSystem extends DrawingPanel {
  override def paint(g: Graphics2D): Unit = {
    super.paint(g)

    g.setColor(new Color(200, 200, 200))
    g.setStroke(new BasicStroke)

    for i <- 0 until size.height if i % 20 == 0 do {
      g.drawLine(0, i, size.width, i)
      g.drawString((i / 20).toString, 0, i)
    }

    for i <- 0 until size.width if i % 20 == 0 do {
      g.drawLine(i, 0, i, size.height)
      g.drawString((i / 20).toString, i, 10)
    }
  }
}

/** Writes the name of the shape besides them on drawing panel */
trait ShowNameLabels extends DrawingPanel {
  override def paint(g: Graphics2D): Unit = {
    super.paint(g)

    g.setColor(new Color(200, 200, 200))
    g.setStroke(new BasicStroke)

    for shape <- state.shapes.now do
      g.drawString(shape.toString, shape.start.x, shape.start.y)
  }
}
