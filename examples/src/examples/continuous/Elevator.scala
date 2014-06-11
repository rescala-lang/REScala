package examples.continuous

import examples.elevator._
import rescala.events._
import rescala.SignalSynt
import rescala.Var
import rescala.Signal
import makro.SignalMacro.{SignalM => Signal}
import scala.swing._
import examples.datastructures._
import rescala.commons.time._


class Elevator(val nFloors : Int) {
	// some constants,
	val FloorHeight = 120
	val FloorStart = 10
	val FloorPos = Iterable.iterate(FloorStart, nFloors)(_ + FloorHeight).toList
	
	// these need to be fine tuned for the system to be 'stable'
	val MinSpeed = 0.5
	val MaxSpeed = 50.0
	val MaxAccel = 50.0
	val BreakDist = 27.5	

	// expose this event to the outside
	val callToFloor = new ImperativeEvent[Int]
	val queue = SQueue(1)
	callToFloor += {queue enqueue _}
	
	// create a timer
	val time = Timer(0)
	
	// Define Signals describing state and behavior of the elevator
	val destination = Signal {
	  queue.head() match {
	    case None => position()
	    case Some(target) => FloorPos(target)
	  }
	}
	val speed = time integrate acceleration() 
	val pos = time integrate speed()
	val position = Signal {pos().asInstanceOf[Int]} // discretize position for drawing
	val distance = Signal { destination() - position() }
	val direction = Signal { math.signum(distance()) }
	val break = Signal { math.abs(distance()) <= BreakDist }
	val stopped = Signal { math.abs(speed()) < MinSpeed }
	val reached = Signal { stopped() && position() == destination() }
	
	val acceleration : Signal[Double] = Signal {
	  if(break()){
	    if(stopped()) 0
	    else -MaxAccel * direction()
	  }
	  else if(math.abs(speed()) > MaxSpeed) 0
	  else MaxAccel  * direction()
	}
	
	speed.changed += {println(_)}
	
	val currentFloor = Signal { 
	  val p = position()
	  FloorPos.indexOf(FloorPos.sortBy(f => math.abs(f - p)).head)
	}
	
	val reachedFloor : Event[Int] = reached.changed && {_ == true} map 
		{(_ : Boolean) => currentFloor()}
	
	
	reachedFloor += {_ => queue.dequeue} // move to the next destination
	
	
	def nameOfFloor(i : Int) = (nFloors - i) match {
	    case 1 => "E"
	    case n => "" + n
	}
}

