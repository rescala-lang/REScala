package reswing

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.language.implicitConversions
import scala.swing.Publisher
import scala.swing.Reactor
import scala.swing.Swing
import scala.swing.UIElement
import scala.swing.event.Event

/**
 * Introduces methods to connect a ''Reactive Swing'' value ([[ReSwingValue]])
 * to the corresponding properties and events of the underlying `Swing` component.
 * 
 * For each `ReSwing` value, you can specify
 *  - a `Swing` getter
 *  - a `Swing` setter optionally
 *  - `Swing` events that indicate a value change which should be represented
 *    in the reactive value
 * 
 * Classes that extend this trait can use a declarative syntax to specify
 * the connections between a [[ReSwingValue]] and the `Swing` properties and
 * events, e.g.:
 * 
 * {{{
 * abstract class ReUIElement(
 *   val preferredSize: ReSwingValue[Dimension] = ())
 * extends ReSwingValueConnection {
 *   protected def peer: UIElement
 *   
 *   preferredSize using (peer.preferredSize _, peer.preferredSize_= _, "preferredSize")
 * }
 * }}}
 * 
 * If a value can be read only but not be passed to the constructor of a
 * base class but sub-classes may add the feature to pass the value to their
 * constructor, the connections should be established in a method that can be
 * overridden in sub-classes, e.g.:
 * 
 * {{{
 * abstract class ReUIElement
 * extends ReSwingValueConnection {
 *   protected def peer: UIElement
 *   
 *   val size: ReSwingValue[Dimension] = ()
 *   protected def initSizeValue {
 *     size using (peer.size _, (peer, classOf[UIElementResized]))
 *   }
 *   initSizeValue
 * }
 * 
 * abstract class ReWindow(
 *   override val size: ReSwingValue[Dimension] = ())
 * extends ReUIElement {
 *   protected def peer: Window
 *   
 *   override protected def initSizeValue {
 *     size using (peer.size _, peer.size_= _, (peer, classOf[UIElementResized]))
 *   }
 * }
 * }}}
 */
private[reswing] abstract trait ReSwingValueConnection {
  protected def peer: UIElement
  
  protected implicit def toReSwingValueConnector[T](signal: ReSwingValue[T]) =
    new ReSwingValueConnector(signal)
  
  protected implicit def toChangingProperty(name: String) =
    Left(name): ChangingProperty
  
  protected implicit def toChangingProperty(reaction: (Publisher, Class[_])) =
    Right(reaction): ChangingProperty
  
  /**
   * Represents a `Swing` property that is used to react on value changes
   * to update the reactive value accordingly.
   * This can be either a [[scala.Predef.String]] representing a bound property
   * ([[java.awt.Component.addPropertyChangeListener]]) or
   * a [[scala.swing.Publisher]] and a [[scala.Predef.Class]] representing the
   * event-publishing component and the event type.
   */
  protected type ChangingProperty = Either[String, (Publisher, Class[_])]
  
  final protected class ReSwingValueConnector[T] private[ReSwingValueConnection]
      (value: ReSwingValue[T]) {
    
    /**
     * Connects the [[ReSwingValue]] object to the given `Swing` getter.
     * The value will be updated when any of the given properties change.
     * This can be used for `Swing` properties that are read-only.
     */
    def using(getter: () => T, names: ChangingProperty*): this.type =
      using(getter, None, names)
    
    /**
     * Connects the [[ReSwingValue]] object to the given `Swing` getter and setter.
     * The value will be updated when any of the given properties change and
     * value changes will be propagated to the `Swing` library.
     * This can be used for `Swing` properties that can be read and written.
     */
    def using(getter: () => T, setter: T => Unit, names: ChangingProperty*): this.type =
      using(getter, Some(setter), names)
    
    private def using(getter: () => T, setter: Option[T => Unit],
        names: Seq[ChangingProperty]): this.type = {
      value() = getter()
      if (setter.isDefined)
        value use setter.get
      
      if (value.fixed && setter.isDefined) {
        val set = setter.get
        for (name <- names)
          name match {
            case Left(name) =>
              changingProperties getOrElseUpdate (name, ListBuffer()) += { _ =>
                if (getter() != value.getValue)
                  Swing.onEDT { if (getter() != value.getValue) set(value.getValue) } }
            
            case Right((publisher, reaction)) =>
              reactor listenTo publisher
              changingReactions getOrElseUpdate (reaction, ListBuffer()) += { _ =>
                if (getter() != value.getValue)
                  Swing.onEDT { if (getter() != value.getValue) set(value.getValue) } }
          }
      }
      
      if (!value.fixed)
        value init { _ =>
          value() = getter()
          for (name <- names)
            name match {
              case Left(name) =>
                changingProperties getOrElseUpdate (name, ListBuffer()) += { _ =>
                  value() = getter() }
              
              case Right((publisher, reaction)) =>
                reactor listenTo publisher
                changingReactions.getOrElseUpdate(reaction, ListBuffer()) += { _ =>
                  value() = getter() }
            }
        }
      
      this
    }
    
    /**
     * Forces the property given by `name` the keep the given value using the
     * given setter, if the [[ReSwingValue]] is set to hold a fixed value,
     * i.e. a value that should not be changed by the library.
     */
    def force[U](name: String, setter: U => Unit, forcedValue: U): this.type = {
      if (value.fixed) {
        setter(forcedValue)
        enforcedProperties += name -> { _ => Swing.onEDT { setter(forcedValue) } }
      }
      this
    }
  }
  
  private val changingReactions = Map.empty[Class[_], ListBuffer[Unit => Unit]]
  private val changingProperties = Map.empty[String, ListBuffer[Unit => Unit]]
  private val enforcedProperties = Map.empty[String, Unit => Unit]
  
  private val reactor = new Reactor {
    reactions += { case e: Event =>
      for (signals <- changingReactions get e.getClass; signal <- signals)
        signal()
    }
  }
  
  peer.peer.addPropertyChangeListener(new java.beans.PropertyChangeListener {
    def propertyChange(e: java.beans.PropertyChangeEvent) {
      enforcedProperties get e.getPropertyName match {
        case Some(setter) => setter()
        case _ => changingProperties.get(e.getPropertyName) match {
          case Some(signals) => for (signal <- signals) signal()
          case _ =>
        }
      }
    }
  })
}
