package examples.timer
import scala.events.behaviour._
import scala.events._
import scala.events.behaviour.Timer


object SnapshotTest extends Application {
	val simulationTime = Timer.create(50)
	val sampleTime = Timer.create(1000)
	
	import scala.events.behaviour.SignalConversions._
	
	lazy val speed : Signal[Double] = Signal { 3.0 }
	lazy val position = simulationTime integral 3.0
	lazy val sampled = position snapshot sampleTime.tick
	
	position.changed += {_ => print('.')}
	sampled.changed += {println(_)}
	
	// position.changed += {_ => println(sampled())}
	Timer.runAll
}