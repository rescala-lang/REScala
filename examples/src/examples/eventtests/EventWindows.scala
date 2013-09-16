package examples.eventtests
import react.SignalSynt
import react.events._
import react.Var
import react.Signal
import macro.SignalMacro.{SignalM => Signal}

object EventWindows extends Application {
	
    val e = new ImperativeEvent[Double]    
    
	val all = e.list()
	val window = e.last(5)
	val mean = Signal { window().sum / (window().length + 1)}
	
	mean.changed += {println(_)}
	
	
	e(1); e(2); e(3); e(4); e(5); e(6)
}