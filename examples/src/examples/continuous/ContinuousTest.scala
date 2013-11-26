package examples.continuous

import react.events.ImperativeEvent
import react.SignalSynt
import react.Var
import react.Signal
import macro.SignalMacro.{SignalM => Signal}
import react.time.Timer


object ContinuousTest extends Application {
  val timer = Timer.create(0)
  
  timer.after(5.0) += { _ => println("5 seconds have passed") }
  
  timer.after(10) += { _ =>
    val local = timer.localTime
    local.changed += {(t : Double) => println("The local time after 10 seconds is is " + t)}
  }

  Timer.runAll
}