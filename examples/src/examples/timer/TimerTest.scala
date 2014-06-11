package examples.timer
import react.commons.time._
import macro.SignalMacro.{SignalM => Signal}
import react.conversions.SignalConversions._

object TimerTest extends App {
  
  val timer1 = Timer(250)
  val timer2 = Timer(500)
  
  timer1.tick += {a => println("timer1. delta=" + a)}
  timer2.tick += {a => println("timer2. delta=" + a)}
  
  // val both = timer1.tock and timer2.tock
  // both += {_ => println("both")}
  
  val either = timer1.tick || timer2.tick
  either += {a => println("either. delta=" + a)}
  
  Timer.runAll
}