class Events {

  // defines which shape is drawn next for example
  // reshapes.figures.Line if nextShape contains reference to line
  val nextShape: Var[Shape] = new Var(new Line)
  // contains the currently selected shape which is, for example,
  // moved around or edited (size/stroke)
  val selectedShape: Var[Shape] = new Var(null)
  // stores all currently drawn shapes
  val allShapes: Var[List[Shape]] = new Var(List[Shape]())
  // currently selected stroke width
  val strokeWidth: Var[Int] = new Var(1)
  // currently selected stroke color
  val color: Var[Color] = new Var(Color.BLACK)
  // stores all executed commands
  val Commands: Var[List[Command]] = new Var(List[Command]())
  // stores the filename after saving
  val fileName: Var[String] = new Var("unnamed")

  var mode: EditingMode = Drawing()
  // event for changes in drawing mode between drawing shapes
  // and selecting shapes
  val modeChange = nextShape.changed || selectedShape.changed

  // event which describes cases where a redraw is necassary
  val canvasChange = selectedShape.changed || allShapes.changed
    || modeChange || strokeWidth.changed || color.changed

  nextShape.changed += (shape => {
    shape.strokeWidth = strokeWidth.getValue
    shape.color = color.getValue
    allShapes.getValue map (x => x.selected = false)
    mode = Drawing()
  })

  selectedShape.changed += (shape => {
    allShapes.getValue map (x => x.selected = false)
    if (shape != null) {
      shape.selected = true
      mode = Selection()
    } else {
      mode = Drawing()
    }
  })

  strokeWidth.changed += (width => {
    if (selectedShape.getValue != null) {
      selectedShape.getValue.strokeWidth = width
    }
  })

  color.changed += (newColor => {
    if (selectedShape.getValue != null) {
      selectedShape.getValue.color = newColor
    }
  })
}
