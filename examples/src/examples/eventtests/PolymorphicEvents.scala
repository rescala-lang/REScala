package examples.eventtests
import react.SignalSynt
import react.events._
import react.Var
import react.Signal
import macro.SignalMacro.{SignalM => Signal}

class Keyboard {
	protected lazy val press : Event[String] = new ImperativeEvent[String]
	def keyPressed = press
	protected lazy val something = Signal { 0 }
}

class Numpad extends Keyboard {
  //lazy val keyPressed = super.keyPressed && (_ => true)
  override def keyPressed = super.keyPressed && (_ => true)
  override lazy val something = Signal {1}
}