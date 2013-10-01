package reswing

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.language.implicitConversions
import scala.swing.Publisher
import scala.swing.Swing
import scala.swing.UIElement
import scala.swing.event.Event

private[reswing] abstract trait ReSwingValueConnection {
  protected def peer: UIElement
  
  protected implicit def toReSwingValueConnector[T](signal: ReSwingValue[T]) =
    new ReSwingValueConnector(signal)
  
  protected implicit def toChangingProperty(name: String) =
    Left(name): ChangingProperty
  
  protected implicit def toChangingProperty(reaction: (Publisher, Class[_])) =
    Right(reaction): ChangingProperty
  
  protected type ChangingProperty = Either[String, (Publisher, Class[_])]
  
  final protected class ReSwingValueConnector[T] private[ReSwingValueConnection]
      (value: ReSwingValue[T]) {
    
    def using(getter: () => T, names: ChangingProperty*): this.type =
      using(getter, None, names)
      
    def using(getter: () => T, setter: T => Unit, names: ChangingProperty*): this.type =
      using(getter, Some(setter), names)
    
    private def using(getter: () => T, setter: Option[T => Unit],
        names: Seq[ChangingProperty]): this.type = {
      setter match {
        case Some(setter) => value() = (getter(), setter)
        case _ => value() = getter()
      }
      
      for (name <- names)
        name match {
          case Left(name) =>
            val changingProperty = changingProperties.getOrElseUpdate(name, ListBuffer())
            if (!value.fixed)
              changingProperty += { _ => value() = getter() }
            else if (setter.isDefined)
              changingProperty += { _ => Swing.onEDT { if (getter() != value.getValue) setter.get(value.getValue) } }
          
          case Right((publisher, reaction)) =>
            publisher.listenTo(publisher)
            publisher.reactions += reactionListener
            
            val changingReaction = changingReactions.getOrElseUpdate(reaction, ListBuffer())
            if (!value.fixed)
              changingReaction += { _ => value() = getter() }
            else if (setter.isDefined)
              changingReaction += { _ => Swing.onEDT { if (getter() != value.getValue) setter.get(value.getValue) } }
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
