package reswing

import scala.events.behaviour.Signal
import scala.events.ImperativeEvent
import scala.swing.Label
import scala.swing.Alignment
import scala.swing.Swing.EmptyIcon
import javax.swing.Icon

class ReLabel(text0: String, icon0: Icon, align0: Alignment.Value) extends ReComponent {
  override protected lazy val peer = new Label(text0, icon0, align0) with LabelMixin

  protected trait LabelMixin extends Label with ComponentMixin {
    val reText = new ReactiveWrapper(text_=, text)
    override def text_=(s : String) {
      super.text = s
      reText.value = s
    }
  }
  
  def this() = this("", EmptyIcon, Alignment.Center)
  def this(s: String) = this(s, EmptyIcon, Alignment.Center)
  def this(s: Signal[String]) { this("", EmptyIcon, Alignment.Center); text = s }
  def this(s: String, a: Alignment.Value) = this(s, EmptyIcon, a)
  def this(s: Signal[String], a: Alignment.Value) { this("", EmptyIcon, a); text = s }
  
  def text = peer.reText.signal
  def text_=(s: Signal[String]) = peer.reText.signal = s
}

object ReLabel {
  implicit def toLabel(input : ReLabel) : Label = input.peer
}
