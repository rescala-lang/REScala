package reswing

import scala.language.implicitConversions
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.TextArea
import scala.swing.event.ValueChanged
import react.events.Event

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
    columns: Int = 0)
  extends
    ReTextComponent(text, editable, `caret.position`, `caret.markDot`,
                    `caret.visible`, `caret.selectionVisible`,
                    `caret.blinkRate`, `caret.color`,
                    cut, copy, paste, selectAll,
                    background, foreground, font, enabled,
                    minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer =
    new TextArea(null, rows, columns) with ComponentMixin
  
  tabSize using (peer.tabSize _, peer.tabSize_= _, "tabSize")
  lineWrap using (peer.lineWrap _, peer.lineWrap_= _, "lineWrap")
  charWrap using (peer.charWrap _, peer.charWrap_= _, "wrapStyleWord")
  
  val lineCount = ReSwingValue using (peer.lineCount _, classOf[ValueChanged])
}

object ReTextArea {
  implicit def toTextArea(component: ReTextArea): TextArea = component.peer
}
