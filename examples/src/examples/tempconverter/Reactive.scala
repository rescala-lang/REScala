package examples.tempconverter

// Wrap a bit of Swing, as if it were an FRP library

// Escala lib + behaviour extensions

import react.events.ImperativeEvent
import react.SignalSynt
import react.Var
import react.Signal
import macro.SignalMacro.{SignalM => Signal}

// Scala swing events
import swing._
import event._


// could we actually use Reactive[Any] and use toString?
trait ReactiveText extends Reactor {
  protected lazy val userSet = Var(text)
  
  def text : String
  def text_=(s : String)
  // in signal
  def text_=(value : Signal[String]) {
    this.text_=(value())
    value.changed += {(t : String) => this.text_=(t)}
  }  
  // out signal
  lazy val text_out = Signal {userSet()}
  reactions += { case EditDone(_) => userSet() = text}
}

class ReactiveTextfield extends TextField with ReactiveText {
  listenTo(this)
}
class ReactiveLabel extends Label with ReactiveText
class ReactiveButton extends Button with ReactiveText {
	// wrap the event to escala
	val clicked = new ImperativeEvent[ButtonClicked]
	reactions += { case c @ ButtonClicked(_) => clicked(c) }
}

