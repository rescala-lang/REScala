package texteditor.events

import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position
import scala.swing.Dimension
import scala.swing.GridPanel
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.ScrollPane
import scala.swing.SimpleSwingApplication

import reswing.ReButton
import reswing.ReLabel

object Application extends SimpleSwingApplication {
  // event-based components (set initial value manually)
  val textArea = new TextArea("Lorem ipsum dolor sit amet\nconsectetur adipisicing elit\nsed do eiusmod")
  
  val positionLabel = new ReLabel({
    val pos = textArea.caret.position
    "Ln " + (pos.row + 1) + " : " + textArea.lineCount + "    Col " + (pos.col + 1)})
  val selectionLabel = new ReLabel("Sel " + textArea.selected.size)
  val charCountLabel = new ReLabel("Ch " + textArea.charCount)
  val wordCountLabel = new ReLabel("Words " + textArea.wordCount)
  
  textArea.caret.changed += { _ => //#HDL
    val pos = textArea.caret.position
    (positionLabel: Label).text = "Ln " + (pos.row + 1) + " : " + textArea.lineCount + "    Col " + (pos.col + 1)
  }
  textArea.selectionChanged += { it => (selectionLabel: Label).text = "Sel " + it.size } //#HDL
  textArea.charCountChanged += { count => (charCountLabel: Label).text = "Ch " + count } //#HDL
  textArea.wordCountChanged += { count => (wordCountLabel: Label).text = "Words " + count } //#HDL
  
  val selectAllButton = new ReButton("Select All") //#EVT
  selectAllButton.clicked += { _ => textArea.selectAll; textArea.requestFocus } //#HDL
  
  val copyButton = new ReButton("Copy") //#EVT
  copyButton.clicked += { _ => textArea.copy; textArea.requestFocus } //#HDL
  
  val pasteButton = new ReButton("Paste") //#EVT
  pasteButton.clicked += { _ => textArea.paste; textArea.requestFocus } //#HDL
  
  // layout
  def top = new MainFrame {
    title = "TextEditor (events)"
    preferredSize = new Dimension(500, 500)
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
