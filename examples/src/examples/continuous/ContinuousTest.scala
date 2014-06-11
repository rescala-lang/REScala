package examples.continuous

import rescala.events.ImperativeEvent
import rescala.SignalSynt
import rescala.Var
import rescala.Signal
import makro.SignalMacro.{SignalM => Signal}
import rescala.time.Timer


object ContinuousTest extends Application {
  val timer = Timer.create(0)
  
  timer.after(5.0) += { _ => println("5 seconds have passed") }
  
  timer.after(10) += { _ =>
    val local = timer.localTime
    local.changed += {(t : Double) => println("The local time after 10 seconds is is " + t)}
  }

  Timer.runAll
}