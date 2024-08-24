package ex2013reswing

import scala.swing.event.ValueChanged
import scala.swing.{Color, Dimension, Font, TextArea}

@scala.annotation.nowarn("msg=shadows field")
class ReTextArea(
    text: ReSwingValue[String] = (),
    editable: ReSwingValue[Boolean] = (),
    `caret.position`: ReSwingValue[Int] = (),
    `caret.markDot`: ReSwingValue[(Int, Int)] = (),
    `caret.visible`: ReSwingValue[Boolean] = (),
    `caret.selectionVisible`: ReSwingValue[Boolean] = (),
    `caret.blinkRate`: ReSwingValue[Int] = (),
    `caret.color`: ReSwingValue[Color] = (),
    val tabSize: ReSwingValue[Int] = (),
    val lineWrap: ReSwingValue[Boolean] = (),
    val charWrap: ReSwingValue[Boolean] = (),
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
    rows: Int = 0,
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
  override protected lazy val peer: TextArea & ComponentMixin =
    new TextArea(null, rows, columns) with ComponentMixin

  tabSize.using({ () => peer.tabSize }, peer.tabSize_=, "tabSize")
  lineWrap.using({ () => peer.lineWrap }, peer.lineWrap_=, "lineWrap")
  charWrap.using({ () => peer.charWrap }, peer.charWrap_=, "wrapStyleWord")

  val lineCount = ReSwingValue.using({ () => peer.lineCount }, classOf[ValueChanged])
}

object ReTextArea {
  implicit def toTextArea(component: ReTextArea): TextArea = component.peer
}
