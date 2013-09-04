package examples.timer
import react._
import macro.SignalMacro.{SignalM => Signal}
import react.conversions.SignalConversions._
import react.time.Timer

object TimerTest extends Application {
  
  val timer1 = Timer.create(250)
  val timer2 = Timer.create(500)
  
  timer1.tick += {a => println("timer1. delta=" + a)}
  timer2.tick += {a => println("timer2. delta=" + a)}
  
  // val both = timer1.tock and timer2.tock
  // both += {_ => println("both")}
  
  println("XX")
  
  val either = timer1.tock || timer2.tock
  either += {_ => println("either")}
  
  Timer.runAll
}