package reswing

import java.awt.Dimension

import scala.events.ImperativeEvent
import scala.swing.TextComponent
import scala.swing.event.CaretUpdate
import scala.swing.event.ValueChanged

class ReTextComponent extends ReComponent {
  override protected lazy val peer = new TextComponent with ComponentMixin
  
  val text: ImperativeSignal[String] = ImperativeSignal.noSignal
  connectSignal(text, peer text, peer text_=)
  
  val selected: ImperativeSignal[String] = ImperativeSignal.noSignal
  
  val valueChanged = new ImperativeEvent[ValueChanged]
  peer.reactions += {
    case e @ ValueChanged(_) =>
      text(peer text)
      valueChanged(e)
  }
  
  peer.caret.reactions += {
    case e @ CaretUpdate(_) =>
      selected(peer selected)
  }
  
  class ReCaret {
    protected lazy val peer = ReTextComponent.this.peer.caret
    
    val dot: ImperativeSignal[Int] = ImperativeSignal.noSignal(peer dot)
    val mark: ImperativeSignal[Int] = ImperativeSignal.noSignal(peer mark)
    val position: ImperativeSignal[Int] = ImperativeSignal.noSignal(peer position)
  
    val caretUpdate = new ImperativeEvent[CaretUpdate]
    peer.reactions += {
      case e @ CaretUpdate(_) =>
        dot(peer dot)
        mark(peer mark)
        position(peer position)
        caretUpdate(e)
    }
  }
  
  object ReCaret {
    implicit def toCaret(input : ReCaret) = input.peer
  }
  
  object caret extends ReCaret
}

object ReTextComponent {
  implicit def toTextComponent(input : ReTextComponent) : TextComponent = input.peer
  
  def apply(
      minimumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      maximumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal) = {
    def minimumSize0 = minimumSize
    def maximumSize0 = maximumSize
    def preferredSize0 = preferredSize
    new ReLabel {
      override lazy val minimumSize = minimumSize0
      override lazy val maximumSize = maximumSize0
      override lazy val preferredSize = preferredSize0
    }: ReLabel
  }
}
