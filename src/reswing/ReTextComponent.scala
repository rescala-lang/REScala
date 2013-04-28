package reswing

import scala.events.behaviour.Var
import scala.events.behaviour.Signal
import scala.events.ImperativeEvent
import scala.swing.TextComponent
import scala.swing.event.CaretUpdate
import scala.swing.event.ValueChanged

class ReTextComponent extends ReComponent {
  override protected lazy val peer = new TextComponent with TextComponentMixin
  
  protected trait TextComponentMixin extends TextComponent with ComponentMixin {
    val reText = new ReactiveWrapper(text_=, text)
    val reSelected = new ReactiveWrapper(null, selected)
    
    class ReCaret extends Caret {
      private[reswing] val reDot = new ReactiveWrapper(dot_=, dot)
      private[reswing] val reMark = new ReactiveWrapper(null, mark)
      private[reswing] val rePosition = new ReactiveWrapper(position_=, position)
    }
    
    object reCaret extends ReCaret
  }
  
  def text = peer.reText.signal
  def text_=(s: Signal[String]) = peer.reText.signal = s
  
  def selected = peer.reSelected.signal
  
  val valueChanged = new ImperativeEvent[ValueChanged]
  peer.reactions += {
    case e @ ValueChanged(_) =>
      peer.reText.value = peer.text
      valueChanged(e)
  }
  
  peer.reCaret.reactions += {
    case e @ CaretUpdate(_) => {
      if (peer.reSelected.value != peer.selected)
    	  peer.reSelected.value = peer.selected
    }
  }
  
  class ReCaret {
    protected lazy val peer = ReTextComponent.this.peer.reCaret
    
    def dot = peer.reDot.signal
    def dot_=(p: Signal[Int]) = peer.reDot.signal = p
    
    def mark = peer.reMark.signal
    
    def position = peer.rePosition.signal
    def position_=(p: Signal[Int]) = peer.rePosition.signal = p
    
    val caretUpdate = new ImperativeEvent[CaretUpdate]
    peer.reactions += {
      case e @ CaretUpdate(_) => {
        peer.reDot.value = peer.dot
        peer.reMark.value = peer.mark
        peer.rePosition.value = peer.position
        caretUpdate(e)
      }
    }
  }
  
  object ReCaret {
    implicit def toCaret(input : ReCaret) = input.peer
  }
  
  object caret extends ReCaret
}

object ReTextComponent {
  implicit def toTextComponent(input : ReTextComponent) : TextComponent = input.peer
}
