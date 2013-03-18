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
  
  textArea.reactions += {
    case e @ ValueChanged(_) => countLabel.text = "Ch " + textArea.charCount
  }
  
  textArea.caret.reactions += {
    case e @ CaretUpdate(_) => {
      val pos = textArea.caret.position
      positionLabel.text = "Ln " + (pos.row + 1) + " : " + textArea.lineCount + "    Col " + (pos.col + 1)
      selectionLabel.text = "Sel " + textArea.selected.length
    }
  }
  
  val positionLabel = new Label
  val selectionLabel = new Label
  val countLabel = new Label
  
  val button = new Button("Select All")
  button.reactions += {
    case e @ ButtonClicked(_) => textArea.selectAll; textArea.requestFocus
  }
  
  // trigger initial events manually
  textArea.reactions(new ValueChanged(textArea))
  textArea.caret.reactions(new CaretUpdate(textArea))
  
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
