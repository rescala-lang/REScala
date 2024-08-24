package ex201x.reswingexamples.reshapes.figures

import ex201x.reswingexamples.reshapes.drawing.DrawingSpaceState

import java.awt.{Color, Graphics2D, Point}

@scala.annotation.nowarn("msg=shadows field")
class Line(
    drawingSpaceState: DrawingSpaceState,
    strokeWidth: Int = 1,
    color: Color = Color.BLACK,
    current: Int = 0,
    path: List[Point] = List.empty
) extends Shape(drawingSpaceState, strokeWidth, color, current, path)
    with Movable
    with Resizable {

  override def doDraw(g: Graphics2D) =
    g.drawLine(start.x, start.y, end.x, end.y)

  override def toLines() =
    List((start, end))

  override def copy(
      drawingSpaceState: DrawingSpaceState,
      strokeWidth: Int,
      color: Color,
      current: Int,
      path: List[Point]
  ) =
    new Line(drawingSpaceState, strokeWidth, color, current, path)
}
