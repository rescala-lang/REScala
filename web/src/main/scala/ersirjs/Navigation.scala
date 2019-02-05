package ersirjs

import org.scalajs.dom
import org.scalajs.dom.raw.KeyboardEvent
import rescala.default.{Event, Evt, scheduler}
import rescala.reactives.Events

object Navigation {
  sealed trait Navigate
  case object Next extends Navigate
  case object Prev extends Navigate
  case class Mode(i: Int) extends Navigate


  val (handleKeypress, _) = Events.fromCallback[KeyboardEvent](dom.document.onkeydown = _)
  val navigate = Evt[Navigate]
  val keypressNavigations = handleKeypress.map(_.key).collect {
    case "ArrowLeft" | "a" | "," => Prev
    case "ArrowRight" | "d" | "." => Next
    case n if n.matches("""^\d+$""") => Mode(n.toInt)
  }

  val navigationEvents: Event[Navigate] = keypressNavigations || navigate

}
