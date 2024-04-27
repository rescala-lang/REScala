package reswing.texteditor.signalsAndEventsFromImperative

import reactives.default.*
import reswing.{ReButton, ReLabel, ReTextArea}

import scala.math.min
import scala.swing.BorderPanel.Position
import scala.swing.{BorderPanel, Dimension, GridPanel, MainFrame, ScrollPane, SimpleSwingApplication}

object ApplicationSwingTextArea extends SimpleSwingApplication {
  // reactive components
  val textArea = new ReTextArea("Lorem ipsum dolor sit amet\nconsectetur adipisicing elit\nsed do eiusmod")

  val positionLabel = new ReLabel(Signal {
    val pos  = min(textArea.caret.position.value, textArea.text.value.length())
    val line = textArea.peer.getLineOfOffset(pos);
    val col  = pos - textArea.peer.getLineStartOffset(line);
    "Ln " + (line + 1) + " : " + textArea.lineCount.value + "    Col " + (col + 1)
  })

  val selectionLabel = new ReLabel(
    Signal { "Sel " + (if (textArea.selected.value != null) textArea.selected.value.length() else 0) }
  )

  val charCountLabel = new ReLabel(Signal { "Ch " + textArea.text.value.length() })

  val wordCountLabel = new ReLabel(Signal { "Words " + textArea.text.value.length() })

  val selectAllButton = new ReButton("Select All")
  selectAllButton.clicked observe { _ =>
    textArea.selectAll(); textArea.requestFocus()
  }

  val copyButton = new ReButton("Copy")
  copyButton.clicked observe { _ =>
    textArea.copy(); textArea.requestFocus()
  }

  val pasteButton = new ReButton("Paste")
  pasteButton.clicked observe { _ =>
    textArea.paste(); textArea.requestFocus()
  }

  // layout
  def top =
    new MainFrame {
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
