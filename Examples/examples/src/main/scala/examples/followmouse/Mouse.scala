package examples.followmouse

import scala.swing.Reactor
import scala.swing.Component
import rescala._
import rescala._
import rescala._
import rescala._
import rescala._
import java.awt.Point
import scala.swing.event._
import scala.swing.Reactions

class Mouse {

	 /* EScala events */
	val mouseMovedE = Evt[Point]
	val mousePressedE = Evt[Point]
	val mouseDraggedE = Evt[Point]
	val mouseReleasedE = Evt[Point]


	/* Compose reactive values */
	val mouseChangePosition = mouseMovedE || mouseDraggedE
	val mousePressedOrReleased = mousePressedE || mouseReleasedE
	val position: Signal[Point] = mouseChangePosition.latest(new Point(0, 0))
	val pressed: Signal[Boolean] = mousePressedOrReleased.toggle(Signal{false}, Signal{true}) // TODO: solve this more robust

	/* Scala swing reaction */
	val react: scala.swing.Reactions.Reaction =  {
        case e: MouseMoved => { mouseMovedE(e.point) }
        case e: MousePressed => mousePressedE(e.point)
        case e: MouseDragged => { mouseDraggedE(e.point) }
        case e: MouseReleased => mouseReleasedE(e.point)
     }

}
