package examples.continuous

import rescala.events.ImperativeEvent
import rescala.SignalSynt
import rescala.Var
import rescala.Signal
import makro.SignalMacro.{SignalM => Signal}
import rescala.commons.time._


object ContinuousTest extends App {
  val timer = Timer(Time.s(1.0))

  timer.after(Time.s(4.0)) += { _ => println("5 seconds have passed") }

  timer.after(Time.s(9.0)) += { _ =>
    val local = timer.localTime
    local.changed += {(t : Time) => println("The local time after 10 seconds is is " + t)}
  }

  Timer.runAll
}
