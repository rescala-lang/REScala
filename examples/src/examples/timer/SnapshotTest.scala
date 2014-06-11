package examples.timer
import react._
import macro.SignalMacro.{SignalM => Signal}
import react.commons.time._


object SnapshotTest extends App {
	val simulationTime = Timer(50)
	val sampleTime = Timer(1000)
	
	// import react.conversions.SignalConversions._
	
	val speed : Signal[Double] = Signal { 3.0 }
	val position = simulationTime integral speed // 3.0
	val sampled = position snapshot sampleTime.tick
	
	position.changed += {_ => print('.')}
	sampled.changed += {println(_)}
	
	Timer.runAll
}