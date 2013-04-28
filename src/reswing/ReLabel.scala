package reswing

import java.awt.Dimension

import scala.swing.Alignment
import scala.swing.Label
import scala.swing.Swing.EmptyIcon

import javax.swing.Icon
import reswing.ImperativeSignal.toSignal

class ReLabel(
    val text: ImperativeSignal[String] = ImperativeSignal.noSignal,
    icon: Icon = EmptyIcon,
    align: Alignment.Value = Alignment.Center,
    minimumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
    maximumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
    preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal)
  extends ReComponent(
    minimumSize = minimumSize,
    maximumSize = maximumSize,
    preferredSize = preferredSize) {
  
  override protected lazy val peer = new Label(text getValue, icon, align) with LabelMixin

  protected trait LabelMixin extends Label with ComponentMixin {
    override def text_=(s : String) {
      super.text = s
      ReLabel.this.text(s)
    }
  }
  
  connectSignal(text, peer text, peer text_=)
}

object ReLabel {
  implicit def toLabel(input : ReLabel) : Label = input.peer
}
