package reswing

import scala.swing.{Alignment, Color, Dimension, Font, TextField}
import scala.swing.event.EditDone

class ReTextField(
    text: ReSwingValue[String] = (),
    editable: ReSwingValue[Boolean] = (),
    `caret.position`: ReSwingValue[Int] = (),
    `caret.markDot`: ReSwingValue[(Int, Int)] = (),
    `caret.visible`: ReSwingValue[Boolean] = (),
    `caret.selectionVisible`: ReSwingValue[Boolean] = (),
    `caret.blinkRate`: ReSwingValue[Int] = (),
    `caret.color`: ReSwingValue[Color] = (),
    val horizontalAlignment: ReSwingValue[Alignment.Value] = (),
    cut: ReSwingEvent[Unit] = (),
    copy: ReSwingEvent[Unit] = (),
    paste: ReSwingEvent[Unit] = (),
    selectAll: ReSwingEvent[Unit] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = (),
    columns: Int = 0
) extends ReTextComponent(
      text,
      editable,
      `caret.position`,
      `caret.markDot`,
      `caret.visible`,
      `caret.selectionVisible`,
      `caret.blinkRate`,
      `caret.color`,
      cut,
      copy,
      paste,
      selectAll,
      background,
      foreground,
      font,
      enabled,
      minimumSize,
      maximumSize,
      preferredSize
    ) {
  override protected lazy val peer: TextField with ComponentMixin =
    new TextField(null, columns) with ComponentMixin

  horizontalAlignment.using({ () => peer.horizontalAlignment }, peer.horizontalAlignment = _, "horizontalAlignment")

  val editDone = ReSwingEvent using classOf[EditDone]
}

object ReTextField {
  implicit def toTextField(component: ReTextField): TextField = component.peer
}
