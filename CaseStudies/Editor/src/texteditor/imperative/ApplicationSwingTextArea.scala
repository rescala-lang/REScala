package texteditor.imperative

import scala.math.min
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

object ApplicationSwingTextArea extends SimpleSwingApplication {
  import scala.swing.TextArea
  import scala.swing.event.CaretUpdate
  import scala.swing.event.ValueChanged

  // imperative components
  val textArea = new TextArea("Lorem ipsum dolor sit amet\nconsectetur adipisicing elit\nsed do eiusmod")
  
  textArea.reactions += {
    case e @ ValueChanged(_) => countLabel.text = "Ch " + textArea.text.length()
  }
  
  textArea.caret.reactions += {
    case e @ CaretUpdate(_) => {
      val pos = min(textArea.caret.position, textArea.text.length())
      val line = textArea.peer.getLineOfOffset(pos);
      val col = pos - textArea.peer.getLineStartOffset(line);
      
      positionLabel.text = "Ln " + (line + 1) + " : " + textArea.lineCount + "    Col " + (col + 1)
      selectionLabel.text = "Sel " + (if (textArea.selected != null) textArea.selected.length else 0)
    }
  }
  
  val positionLabel = new Label
  val selectionLabel = new Label
  val countLabel = new Label
  
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
  
  // trigger initial events manually
  textArea.reactions(new ValueChanged(textArea))
  textArea.caret.reactions(new CaretUpdate(textArea))
  
  // layout
  def top = new MainFrame {
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
        contents += countLabel
      }) = Position.South
    }
  }
}
