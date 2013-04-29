package texteditor.events

import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position
import scala.swing.Dimension
import scala.swing.GridPanel
import scala.swing.MainFrame
import scala.swing.ScrollPane
import scala.swing.SimpleSwingApplication
import reswing.ReButton
import reswing.ReButton.toButton
import reswing.ReComponent.toComponent
import reswing.ReLabel
import reswing.ReLabel.toLabel
import scala.swing.Label

object Application extends SimpleSwingApplication {
  // event-based components (set initial value manually)
  val textArea = new TextArea("Lorem ipsum dolor sit amet\nconsectetur adipisicing elit\nsed do eiusmod")
  
  val positionLabel = ReLabel({
    val pos = textArea.caret.position
    "Ln " + (pos.row + 1) + " : " + textArea.lineCount + "    Col " + (pos.col + 1)})
  val selectionLabel = ReLabel("Sel " + textArea.selected.size)
  val charCountLabel = ReLabel("Ch " + textArea.charCount)
  val wordCountLabel = ReLabel("Words " + textArea.wordCount)
  
  textArea.caret.changed += { _ =>
    val pos = textArea.caret.position
    (positionLabel: Label).text = "Ln " + (pos.row + 1) + " : " + textArea.lineCount + "    Col " + (pos.col + 1)
  }
  textArea.selectionChanged += { it => (selectionLabel: Label).text = "Sel " + it.size }
  textArea.charCountChanged += { count => (charCountLabel: Label).text = "Ch " + count }
  textArea.wordCountChanged += { count => (wordCountLabel: Label).text = "Words " + count }
  
  val selectAllButton = ReButton("Select All")
  selectAllButton.clicked += { _ => textArea.selectAll; textArea.requestFocus }
  
  val copyButton = ReButton("Copy")
  copyButton.clicked += { _ => textArea.copy; textArea.requestFocus }
  
  val pasteButton = ReButton("Paste")
  pasteButton.clicked += { _ => textArea.paste; textArea.requestFocus }
  
  // layout
  def top = new MainFrame {
    title = "TextEditor (events)"
    preferredSize = new Dimension(400, 400)
    contents = new BorderPanel {
      layout(new ScrollPane(textArea)) = Position.Center
      layout(new GridPanel(1, 0) {
        contents += selectAllButton
        contents += copyButton
        contents += pasteButton
      }) = Position.North
      layout(new GridPanel(1, 0) {
        contents += positionLabel
        contents += selectionLabel
        contents += charCountLabel
        contents += wordCountLabel
      }) = Position.South
    }
  }
}
