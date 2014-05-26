package texteditor.imperative

import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position
import scala.swing.Button
import scala.swing.Dimension
import scala.swing.GridPanel
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.ScrollPane
import scala.swing.SimpleSwingApplication
import scala.swing.event.ButtonClicked

object Application extends SimpleSwingApplication {
  // imperative components
  val textArea = new TextArea("Lorem ipsum dolor sit amet\nconsectetur adipisicing elit\nsed do eiusmod")
  textArea.reactions += { case e @ ValueChanged(_) => textAreaValueChanged }
  textArea.caret.reactions += { case e @ CaretUpdate(_) => textAreaCaretUpdated }
  
  def textAreaValueChanged {
    charCountLabel.text = "Ch " + textArea.charCount
    wordCountLabel.text = "Words " + textArea.wordCount
  }
  
  def textAreaCaretUpdated {
    val pos = textArea.caret.position
    positionLabel.text = "Ln " + (pos.row + 1) + " : " + textArea.lineCount + "    Col " + (pos.col + 1)
    selectionLabel.text = "Sel " + textArea.selected.length
  }
  
  val positionLabel = new Label
  val selectionLabel = new Label
  val charCountLabel = new Label
  val wordCountLabel = new Label
  
  val selectAllButton = new Button("Select All")
  selectAllButton.reactions += {
    case e @ ButtonClicked(_) => textArea.selectAll; textArea.requestFocus
  }
  
  val copyButton = new Button("Copy")
  copyButton.reactions += {
    case e @ ButtonClicked(_) => textArea.copy; textArea.requestFocus
  }
  
  val pasteButton = new Button("Paste")
  pasteButton.reactions += {
    case e @ ButtonClicked(_) => textArea.paste; textArea.requestFocus
  }
  
  // update dependent values manually
  textAreaValueChanged
  textAreaCaretUpdated
  
  // layout
  def top = new MainFrame {
    title = "TextEditor (imperative)"
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
