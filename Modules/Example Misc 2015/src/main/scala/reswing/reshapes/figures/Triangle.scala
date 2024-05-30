package reswing.reshapes.figures

import reswing.reshapes.drawing.DrawingSpaceState

import java.awt.{Color, Graphics2D, Point}

class Triangle(
    drawingSpaceState: DrawingSpaceState,
    strokeWidth: Int = 1,
    color: Color = Color.BLACK,
    current: Int = 0,
    path: List[Point] = List.empty
) extends Shape(drawingSpaceState, strokeWidth, color, current, path)
    with Movable
    with Resizable {

  override def doDraw(g: Graphics2D) =
    for (a, b) <- toLines() do
      g.drawLine(a.x, a.y, b.x, b.y)

  override def toLines() =
    List((start, end), (start, new Point(start.x, end.y)), (new Point(start.x, end.y), end))

  override def copy(
      drawingSpaceState: DrawingSpaceState,
      strokeWidth: Int,
      color: Color,
      current: Int,
      path: List[Point]
  ) =
    new Triangle(drawingSpaceState, strokeWidth, color, current, path)
}
