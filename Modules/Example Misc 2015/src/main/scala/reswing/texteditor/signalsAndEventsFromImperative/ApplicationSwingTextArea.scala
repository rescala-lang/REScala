package reswing.texteditor.signalsAndEventsFromImperative

import rescala.default._
import reswing.{ReButton, ReLabel, ReTextArea}

import scala.math.min
import scala.swing.BorderPanel.Position
import scala.swing.{BorderPanel, Dimension, GridPanel, MainFrame, ScrollPane, SimpleSwingApplication}

object ApplicationSwingTextArea extends SimpleSwingApplication {
  // reactive components
  val textArea = new ReTextArea("Lorem ipsum dolor sit amet\nconsectetur adipisicing elit\nsed do eiusmod")

  val positionLabel = new ReLabel(Signal {
    val pos  = min(textArea.caret.position(), textArea.text().length())
    val line = textArea.peer.getLineOfOffset(pos);
    val col  = pos - textArea.peer.getLineStartOffset(line);
    "Ln " + (line + 1) + " : " + textArea.lineCount() + "    Col " + (col + 1)
  })

  val selectionLabel = new ReLabel(
    Signal { "Sel " + (if (textArea.selected() != null) textArea.selected().length() else 0) }
  )

  val charCountLabel = new ReLabel(Signal { "Ch " + textArea.text().length() })

  val wordCountLabel = new ReLabel(Signal { "Words " + textArea.text().length() })

  val selectAllButton = new ReButton("Select All")
  selectAllButton.clicked += { _ =>
    textArea.selectAll(); textArea.requestFocus()
  }

  val copyButton = new ReButton("Copy")
  copyButton.clicked += { _ =>
    textArea.copy(); textArea.requestFocus()
  }

  val pasteButton = new ReButton("Paste")
  pasteButton.clicked += { _ =>
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
