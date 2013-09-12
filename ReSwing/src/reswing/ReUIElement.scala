package reswing

import scala.language.implicitConversions
import scala.swing.Dimension
import scala.swing.Point
import scala.swing.Rectangle
import scala.swing.Swing
import scala.swing.UIElement
import scala.swing.event.UIElementMoved
import scala.swing.event.UIElementResized

abstract class ReUIElement(
    val minimumSize: ReSwingValue[Dimension] = ReSwingValue.noValue,
    val maximumSize: ReSwingValue[Dimension] = ReSwingValue.noValue,
    val preferredSize: ReSwingValue[Dimension] = ReSwingValue.noValue) {
  protected def peer: UIElement
  
  protected implicit def toReSwingValueConnector[T](signal: ReSwingValue[T]) =
    new ReSwingValueConnector(signal)
  
  minimumSize using (peer.minimumSize _, peer.minimumSize_= _, "minimumSize")
  maximumSize using (peer.maximumSize _, peer.maximumSize_= _, "maximumSize")
  preferredSize using (peer.preferredSize _, peer.preferredSize_= _, "preferredSize")
  
  val size: ReSwingValue[Dimension] = peer.size
  val bounds: ReSwingValue[Rectangle] = peer.bounds
  val location: ReSwingValue[Point] = peer.location
  
  peer.listenTo(peer);
  peer.reactions += {
    case e @ UIElementMoved(_) => location() = peer.location
    case e @ UIElementResized(_) => size() = peer.size; bounds() = peer.bounds
  }
  
  protected class ReSwingValueConnector[T](value: ReSwingValue[T]) {
    def using[U](getter: () => T, setter: T => Unit, names: String*) = {
      value(getter(), setter)
      for (name <- names)
        if (value.fixed)
    	  signalProperties += name -> { _ => Swing.onEDT { setter(value.getValue) } }
        else
    	  signalProperties += name -> { _ => value() = getter() }
      this
    }
    
    def force[U](name: String, setter: U => Unit, forcedValue: U) = {
      if (value.fixed) {
        setter(forcedValue)
        enforcedProperties += name -> { _ => Swing.onEDT { setter(forcedValue) } }
      }
      this
    }
  }
  
  private lazy val signalProperties = scala.collection.mutable.Map.empty[String, Unit => Unit]
  private lazy val enforcedProperties = scala.collection.mutable.Map.empty[String, Unit => Unit]
  
  peer.peer.addPropertyChangeListener(new java.beans.PropertyChangeListener {
    def propertyChange(e: java.beans.PropertyChangeEvent) {
      enforcedProperties.get(e.getPropertyName) match {
        case Some(setter) => setter()
        case _ => signalProperties.get(e.getPropertyName) match {
          case Some(signal) => signal()
          case _ =>
        }
      }
    }
  })
}

object ReUIElement {
  implicit def toUIElement(component: ReUIElement): UIElement = component.peer
}
