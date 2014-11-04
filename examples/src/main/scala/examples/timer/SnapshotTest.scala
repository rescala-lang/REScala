package examples.timer
import rescala._
import makro.SignalMacro.{SignalM => Signal}
import rescala.commons.time._


object SnapshotTest extends App {
	val simulationTime = Timer(50)
	val sampleTime = Timer(1000)
	
	// import rescala.conversions.SignalConversions._
	
	val speed : Signal[Time] = Signal { 3.0 }
	val position = simulationTime integral speed // 3.0
	val sampled = position snapshot sampleTime.tick
	
	position.changed += {_ => print('.')}
	sampled.changed += {println(_)}
	
	Timer.runAll
}