package examples.timer
import scala.events.behaviour._
import scala.events._
import scala.events.behaviour._


object TimerTest extends Application {
  
  val timer1 = Timer.create(250)
  val timer2 = Timer.create(500)
  
  //timer1.tick += {a => println("timer1. delta=" + a)}
  //timer2.tick += {a => println("timer2. delta=" + a)}
  
  val both = timer1.tock and timer2.tock
  both += {_ => println("both")}
  
  val either = timer1.tock || timer2.tock
  either += {_ => println("either")}
  
  Timer.runAll
}