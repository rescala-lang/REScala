package examples

import java.awt.Point

import rescala.default._

import scala.swing.event._

class Mouse {

  /* EScala events */
  val mouseMovedE    = Evt[Point]()
  val mousePressedE  = Evt[Point]()
  val mouseDraggedE  = Evt[Point]()
  val mouseReleasedE = Evt[Point]()
  /* Compose reactive values */
  val mouseChangePosition     = mouseMovedE || mouseDraggedE
  val mousePressedOrReleased  = mousePressedE || mouseReleasedE
  val position: Signal[Point] = mouseChangePosition.hold(new Point(0, 0))
  val pressed: Signal[Boolean] =
    mousePressedOrReleased.toggle(Signal { false }, Signal { true }) // TODO: solve this more robust

  /* Scala swing reaction */
  val react: scala.swing.Reactions.Reaction = {
    case e: MouseMoved    => { mouseMovedE.fire(e.point) }
    case e: MousePressed  => mousePressedE.fire(e.point)
    case e: MouseDragged  => { mouseDraggedE.fire(e.point) }
    case e: MouseReleased => mouseReleasedE.fire(e.point)
  }

}
