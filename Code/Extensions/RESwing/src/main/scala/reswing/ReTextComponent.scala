package reswing

import scala.language.implicitConversions
import scala.swing.{Color, Dimension, Font, TextComponent}
import scala.swing.event.{CaretUpdate, ValueChanged}

class ReTextComponent(
    val text: ReSwingValue[String] = (),
    val editable: ReSwingValue[Boolean] = (),
    `caret.position`: ReSwingValue[Int] = (),
    `caret.markDot`: ReSwingValue[(Int, Int)] = (),
    `caret.visible`: ReSwingValue[Boolean] = (),
    `caret.selectionVisible`: ReSwingValue[Boolean] = (),
    `caret.blinkRate`: ReSwingValue[Int] = (),
    `caret.color`: ReSwingValue[Color] = (),
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
    preferredSize: ReSwingValue[Dimension] = ())
  extends
    ReComponent(background, foreground, font, enabled,
                minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer = new TextComponent with ComponentMixin

  val selected = ReSwingValue using (peer.selected _, (caret.peer, classOf[CaretUpdate]))

  (text using (peer.text _, peer.text_= _, classOf[ValueChanged])
        force ("editable", peer.editable_= _, false))

  editable using (peer.editable _, peer.editable_= _, "editable")

  cut using (() => peer.cut())
  copy using (() => peer.copy())
  paste using (() => peer.paste())
  selectAll using (() => peer.selectAll())

  class ReCaret(
      val position: ReSwingValue[Int],
      val markDot: ReSwingValue[(Int, Int)],
      val visible: ReSwingValue[Boolean],
      val selectionVisible: ReSwingValue[Boolean],
      val blinkRate: ReSwingValue[Int],
      val color: ReSwingValue[Color]) {
    protected[ReTextComponent] val peer = ReTextComponent.this.peer.caret

    val dot = ReSwingValue using (peer.dot _, (peer, classOf[CaretUpdate]))
    val mark = ReSwingValue using (peer.mark _, (peer, classOf[CaretUpdate]))

    position using (peer.position _, peer.position_= _, (peer, classOf[CaretUpdate]))

    markDot using (
        { () => (peer.mark, peer.dot) },
        { _ match { case(mark, dot) => peer.position = mark; peer.moveDot(dot) } },
        (peer, classOf[CaretUpdate]))

    visible using (peer.visible _, peer.visible_= _)
    selectionVisible using (peer.selectionVisible _, peer.selectionVisible_= _)
    blinkRate using (peer.blinkRate _, peer.blinkRate_= _)
    color using (peer.color _, peer.color_= _)
  }

  object ReCaret {
    implicit def toCaret(caret: ReCaret) = caret.peer
  }

  object caret extends ReCaret(`caret.position`, `caret.markDot`,
      `caret.visible`, `caret.selectionVisible`, `caret.blinkRate`, `caret.color`)
}

object ReTextComponent {
  implicit def toTextComponent(component: ReTextComponent): TextComponent = component.peer
}
