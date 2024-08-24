package ex201x.reswingexamples.texteditor.signalsAndEventsFromImperative

import reactives.default.*
import ex2013reswing.{ReButton, ReLabel}

import scala.swing.BorderPanel.Position
import scala.swing.{BorderPanel, Dimension, GridPanel, MainFrame, ScrollPane, SimpleSwingApplication}

object Application extends SimpleSwingApplication {
  // reactive components
  val textArea = new TextArea("Lorem ipsum dolor sit amet\nconsectetur adipisicing elit\nsed do eiusmod")

  val positionLabel = new ReLabel(Signal {
    val pos = textArea.caret.position.value
    "Ln " + (pos.row + 1) + " : " + textArea.lineCount.value + "    Col " + (pos.col + 1)
  })

  val selectionLabel = new ReLabel(
    Signal { "Sel " + textArea.selected.value.size }
  )

  val charCountLabel = new ReLabel(Signal { "Ch " + textArea.charCount.value })

  val wordCountLabel = new ReLabel(Signal { "Words " + textArea.wordCount.value })

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
      title = "TextEditor (signals0)"
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
