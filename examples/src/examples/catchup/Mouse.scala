package examples.catchup

import scala.swing.Reactor
import scala.swing.Component
import react.events.ImperativeEvent
import react.SignalSynt
import react.Var
import react.Signal
import macro.SignalMacro.{SignalM => Signal}
import java.awt.Point
import scala.swing.event._
import scala.swing.Reactions

class Mouse {
  
	 /* EScala events */
	val mouseMovedE = new ImperativeEvent[Point]()
	val mousePressedE = new ImperativeEvent[Point]()
	val mouseDraggedE = new ImperativeEvent[Point]()
	val mouseReleasedE = new ImperativeEvent[Point]()
	
	
	/* Compose reactive values */
	val mouseChangePosition = mouseMovedE || mouseDraggedE
	val mousePressedOrReleased = mousePressedE || mouseReleasedE
	val position: Signal[Point] = mouseChangePosition.latest(new Point(0, 0))
	val pressed: Signal[Boolean] = mousePressedOrReleased.toggle(Signal{false}, Signal{true}) // TODO: solve this more robust
	
}