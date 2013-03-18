package texteditor.reactive

import scala.events.behaviour.Signal
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position
import scala.swing.Dimension
import scala.swing.GridPanel
import scala.swing.MainFrame
import scala.swing.ScrollPane
import scala.swing.SimpleSwingApplication

import reswing.ReButton
import reswing.ReButton.toButton
import reswing.ReLabel
import reswing.ReLabel.toLabel
import texteditor.reactive.TextArea.toComponent

object Application extends SimpleSwingApplication {
  // reactive components
  val textArea = new TextArea("Lorem ipsum dolor sit amet\nconsectetur adipisicing elit\nsed do eiusmod")
  
  val positionLabel = new ReLabel(Signal {
    val pos = textArea.caret.position()
    "Ln " + (pos.row + 1) + " : " + textArea.lineCount() + "    Col " + (pos.col + 1)
  })
  
  val selectionLabel = new ReLabel(
    Signal { "Sel " + textArea.selected().size })
  
  val countLabel = new ReLabel(Signal { "Ch " + textArea.charCount() })
  
  val button = new ReButton("Select All")
  button.clicked += { _ => textArea.selectAll; textArea.requestFocus }
  
  // layout
  def top = new MainFrame {
    preferredSize = new Dimension(400, 400)
    contents = new BorderPanel {
      layout(new ScrollPane(textArea)) = Position.Center
      layout(button) = Position.North
      layout(new GridPanel(1, 0) {
        contents += positionLabel
        contents += selectionLabel
        contents += countLabel
      }) = Position.South
    }
  }
}
