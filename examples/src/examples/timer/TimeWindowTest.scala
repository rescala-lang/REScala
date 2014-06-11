package examples.timer
import rescala._
import makro.SignalMacro.{SignalM => Signal}
import rescala.conversions.SignalConversions._
import rescala.commons.time._


object TimeWindowTest extends App {
  val timer = Timer(250)
  
  //val eachTwoSeconds = timer.repeatedly(2.0)
  //eachTwoSeconds += {(Unit) => println("two seconds passed")}
  
  val sinWindow = timer.timeWindow(1.0)(Signal{math.sin(timer.time())})  
  sinWindow.changed += {println(_)}
  
  Timer.runAll

}