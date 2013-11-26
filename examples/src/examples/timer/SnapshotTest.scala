package examples.timer
import react._
import macro.SignalMacro.{SignalM => Signal}
import react.time._



object SnapshotTest extends Application {
	val simulationTime = Timer.create(50)
	val sampleTime = Timer.create(1000)
	
	// import react.conversions.SignalConversions._
	
	val speed : Signal[Double] = Signal { 3.0 }
	val position = simulationTime integral speed // 3.0
	val sampled = position snapshot sampleTime.tick
	
	println("aaaaa")
	position.changed += {_ => print('.')}
	sampled.changed += {println(_)}
	
	// position.changed += {_ => println(sampled())}
	println("bbb")
	Timer.runAll
	println("ccc")
}