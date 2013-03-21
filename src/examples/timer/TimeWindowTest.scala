package examples.timer
import scala.events.behaviour._
import scala.events._
import scala.events.behaviour._
import scala.events.behaviour.SignalConversions._


object TimeWindowTest extends Application {
  val timer = Timer.create(250)
  
  //val eachTwoSeconds = timer.repeatedly(2.0)
  //eachTwoSeconds += {(Unit) => println("two seconds passed")}
  
  val sinWindow = timer.timeWindow(1.0)(math.sin(timer.time()))  
  sinWindow.changed += {println(_)}
  
  Timer.runAll

}