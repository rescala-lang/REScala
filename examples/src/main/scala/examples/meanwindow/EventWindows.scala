package examples.meanwindow
import rescala.SignalSynt
import rescala.events._
import rescala.Var
import rescala.Signal
import makro.SignalMacro.{SignalM => Signal}

object EventWindows extends App {
	
    val e = new ImperativeEvent[Double]    
    
	// val all = e.list()
	val window = e.last(5)
	val mean = Signal { 
      window().sum / 
      window().length
    }
	mean.changed += {println(_)}
	
	
	e(2); e(1); e(3); e(4); e(1); e(1)
}