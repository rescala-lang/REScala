package texteditor.signalsAndEventsFromEventsOnly

import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position
import scala.swing.Dimension
import scala.swing.GridPanel
import scala.swing.MainFrame
import scala.swing.ScrollPane
import scala.swing.SimpleSwingApplication

import makro.SignalMacro.{SignalM => Signal}
import reswing.ReButton
import reswing.ReLabel

object Application extends SimpleSwingApplication {
  // reactive components
  val textArea = new TextArea("Lorem ipsum dolor sit amet\nconsectetur adipisicing elit\nsed do eiusmod")
  
  val positionLabel = new ReLabel(Signal { //#SIG
    val pos = textArea.caret.position()
    "Ln " + (pos.row + 1) + " : " + textArea.lineCount() + "    Col " + (pos.col + 1)
  })
  
  val selectionLabel = new ReLabel(Signal { "Sel " + textArea.selected().size }) //#SIG //#IS( // )
  
  val charCountLabel = new ReLabel(Signal { "Ch " + textArea.charCount() })  //#SIG //#IS( // )
  
  val wordCountLabel = new ReLabel(Signal { "Words " + textArea.wordCount() })  //#SIG //#IS( // )
  // TODO: refactoring to signals ?
  val selectAllButton = new ReButton("Select All")  //#EVT
  selectAllButton.clicked += { _ => textArea.selectAll; textArea.requestFocus } //#HDL
  
  val copyButton = new ReButton("Copy")  //#EVT
  copyButton.clicked += { _ => textArea.copy; textArea.requestFocus } //#HDL
  
  val pasteButton = new ReButton("Paste")  //#EVT
  pasteButton.clicked += { _ => textArea.paste; textArea.requestFocus } //#HDL
  
  // layout
  def top = new MainFrame {
    title = "TextEditor (signals1)"
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
