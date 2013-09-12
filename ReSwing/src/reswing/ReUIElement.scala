package reswing

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.language.implicitConversions
import scala.swing.Dimension
import scala.swing.Point
import scala.swing.Publisher
import scala.swing.Rectangle
import scala.swing.Swing
import scala.swing.UIElement
import scala.swing.event.Event
import scala.swing.event.UIElementMoved
import scala.swing.event.UIElementResized

abstract class ReUIElement(
    val minimumSize: ReSwingValue[Dimension] = (),
    val maximumSize: ReSwingValue[Dimension] = (),
    val preferredSize: ReSwingValue[Dimension] = ()) {
  protected def peer: UIElement
  
  protected implicit def toReSwingValueConnector[T](signal: ReSwingValue[T]) =
    new ReSwingValueConnector(signal)
  
  protected implicit def toChangingProperty(name: String) =
    Left(name): ChangingProperty
  
  protected implicit def toChangingProperty(reaction: (Publisher, Class[_])) =
    Right(reaction): ChangingProperty
  
  val size: ReSwingValue[Dimension] = ()
  val bounds: ReSwingValue[Rectangle] = ()
  val location: ReSwingValue[Point] = ()
  
  size using (peer.size _, (peer, classOf[UIElementResized]))
  bounds using (peer.bounds _, (peer, classOf[UIElementResized]))
  location using (peer.location _, (peer, classOf[UIElementMoved]))
  
  minimumSize using (peer.minimumSize _, peer.minimumSize_= _, "minimumSize")
  maximumSize using (peer.maximumSize _, peer.maximumSize_= _, "maximumSize")
  preferredSize using (peer.preferredSize _, peer.preferredSize_= _, "preferredSize")
  
  type ChangingProperty = Either[String, (Publisher, Class[_])]
  
  protected class ReSwingValueConnector[T](value: ReSwingValue[T]) {
    def using(getter: () => T, names: ChangingProperty*) = {
      value() = getter()
      for (name <- names)
        name match {
          case Left(name) =>
            if (!value.fixed)
              changingProperties.getOrElseUpdate(name, ListBuffer()) +=
                { _ => value() = getter() }
          case Right((publisher, reaction)) =>
            if (!value.fixed) {
              publisher.listenTo(publisher)
              publisher.reactions += reactionListener
              changingReactions.getOrElseUpdate(reaction, ListBuffer()) +=
                { _ => value() = getter() }
            }
        }
      this
    }
    
    def using(getter: () => T, setter: T => Unit, names: ChangingProperty*) = {
      value(getter(), setter)
      for (name <- names)
        name match {
          case Left(name) =>
            changingProperties.getOrElseUpdate(name, ListBuffer()) += (
              if (value.fixed)
                { _ => Swing.onEDT { if (getter() != value.getValue) setter(value.getValue) } }
              else
                { _ => value() = getter() })
          case Right((publisher, reaction)) =>
            publisher.listenTo(publisher)
            publisher.reactions += reactionListener
            changingReactions.getOrElseUpdate(reaction, ListBuffer()) += (
              if (value.fixed)
                { _ => Swing.onEDT { if (getter() != value.getValue) setter(value.getValue) } }
              else
                { _ => value() = getter() })
        }
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
  
  private lazy val changingReactions = Map.empty[Class[_], ListBuffer[Unit => Unit]]
  private lazy val changingProperties = Map.empty[String, ListBuffer[Unit => Unit]]
  private lazy val enforcedProperties = Map.empty[String, Unit => Unit]
  
  private lazy val reactionListener = PartialFunction({ e: Event =>
    for (signals <- changingReactions.get(e.getClass))
      for (signal <- signals)
        signal()
  })
  
  peer.peer.addPropertyChangeListener(new java.beans.PropertyChangeListener {
    def propertyChange(e: java.beans.PropertyChangeEvent) {
      enforcedProperties.get(e.getPropertyName) match {
        case Some(setter) => setter()
        case _ => changingProperties.get(e.getPropertyName) match {
          case Some(signals) => for (signal <- signals) signal()
          case _ =>
        }
      }
    }
  })
}

object ReUIElement {
  implicit def toUIElement(component: ReUIElement): UIElement = component.peer
}
