package reswing.reshapes.figures

import java.awt.Color
import java.awt.Graphics2D
import java.awt.Point

import reswing.reshapes.drawing.DrawingSpaceState

class Freedraw(
    drawingSpaceState: DrawingSpaceState,
    strokeWidth: Int = 1,
    color: Color = Color.BLACK,
    current: Int = 0,
    path: List[Point] = List.empty
) extends Shape(drawingSpaceState, strokeWidth, color, current, path)
    with Movable
    with Resizable {

  override def doDraw(g: Graphics2D) =
    for ((a, b) <- toLines())
      g.drawLine(a.x, a.y, b.x, b.y)

  override def toLines() =
    path zip path.tail

  override def copy(
      drawingSpaceState: DrawingSpaceState,
      strokeWidth: Int,
      color: Color,
      current: Int,
      path: List[Point]
  ) =
    new Freedraw(drawingSpaceState, strokeWidth, color, current, path)
}
