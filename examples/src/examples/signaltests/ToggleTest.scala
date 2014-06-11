package examples.signaltests
import scala.events.behaviour._
import scala.events._
import scala.events.behaviour.Timer
import scala.events.behaviour.SignalConversions._

object ToggleTest extends Application {  
  
  val time = Timer.create(200).time
  // toggled will be 42 every other tick
  val toggled = time.changed.toggle(time, 42.0)
  
  toggled.changed += {println(_)}
  
  Timer.runAll
}