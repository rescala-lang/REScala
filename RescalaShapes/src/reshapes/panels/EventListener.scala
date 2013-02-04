package reshapes.panels
import reshapes.Events

trait EventListener {
  def setEvents(newEvents: Events)
}