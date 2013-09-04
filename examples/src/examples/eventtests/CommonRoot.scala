package examples.signaltests
import scala.events.behaviour._
import scala.events._

// Illustrates how events were extended to support += and -= methods for other events

object CommonRoot extends Application {
  val a = new ImperativeEvent[Int]
  val b = new ImperativeEvent[Int]
  
  val either = a || b
  either += (x => println("either: " + x))
  
  val both = a and b
  both += (x => println("both: " + x))
  
  val trigger = new ImperativeEvent[Int]
  trigger +=: a
  trigger +=: b
  trigger(3)
  trigger -=: b
  trigger(3)  
  trigger -=: b
}
