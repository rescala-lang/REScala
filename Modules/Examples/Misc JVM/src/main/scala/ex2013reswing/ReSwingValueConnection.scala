package ex2013reswing

import java.awt.event.{HierarchyEvent, HierarchyListener}
import scala.collection.mutable.{ListBuffer, Map}
import scala.swing.event.Event
import scala.swing.{Publisher, Reactor, Swing, UIElement}

/** Introduces methods to connect a ''Reactive Swing'' value ([[ReSwingValue]])
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
  *   preferredSize.using({() => peer.preferredSize}, peer.preferredSize_=, "preferredSize")
  * }
  * }}}
  *
  * If a value should be read-only, the following syntax can be used:
  *
  * {{{
  * abstract class ReComponent
  * extends ReSwingValueConnection {
  *   protected def peer: Component
  *
  *   val hasFocus = ReSwingValue.using({() => peer.hasFocus}, classOf[FocusGained], classOf[FocusLost])
  * }
  * }}}
  */
private[ex2013reswing] trait ReSwingValueConnection {
  protected def peer: UIElement

  protected implicit def toReSwingValueConnector[T](signal: ReSwingValue[T]): ReSwingValueConnector[T] =
    new ReSwingValueConnector(signal)

  protected implicit def toChangingProperty(name: String): ChangingProperty =
    Left(name): ChangingProperty

  protected implicit def toChangingProperty(reaction: (Publisher, Class[?])): ChangingProperty =
    Right(reaction): ChangingProperty

  protected implicit def toChangingProperty(reaction: Class[?]): ChangingProperty =
    Right((peer, reaction)): ChangingProperty

  protected def ReSwingValue[T]: ReSwingValue[T] = (): ReSwingValue[T]

  /** Represents a `Swing` property that is used to react on value changes
    * to update the reactive value accordingly.
    * This can be either a String representing a bound property
    * (java.awt.Component#addPropertyChangeListener) or
    * a scala.swing.Publisher and a scala.Predef.Class representing the
    * event-publishing component and the event type.
    */
  protected type ChangingProperty = Either[String, (Publisher, Class[?])]

  final protected class ReSwingValueConnector[T] private[ReSwingValueConnection] (value: ReSwingValue[T]) {

    /** Connects the [[ReSwingValue]] object to the given `Swing` getter.
      * The value will be updated when any of the given properties change.
      * This can be used for `Swing` properties that are read-only.
      */
    def using(getter: () => T, names: ChangingProperty*): ReSwingValue[T] =
      using(getter, None, names)

    /** Connects the [[ReSwingValue]] object to the given `Swing` getter and setter.
      * The value will be updated when any of the given properties change and
      * value changes will be propagated to the `Swing` library.
      * This can be used for `Swing` properties that can be read and written.
      */
    def using(getter: () => T, setter: T => Unit, names: ChangingProperty*): ReSwingValue[T] =
      using(getter, Some(setter), names)

    private def using(getter: () => T, setter: Option[T => Unit], names: Seq[ChangingProperty]): ReSwingValue[T] = {
      if value != null then
        inSyncEDT {
          value() = getter()
          value initLazily { _ => inSyncEDT { initReSwingValueConnection() } }

          delayedInitValues += { () =>
            var updatingSwingNotification = false
            if setter.isDefined then {
              val set = setter.get
              value use { v =>
                if updatingSwingNotification then
                  Swing onEDT { set(v) }
                else
                  inSyncEDT { set(v) }
              }

              if value.fixed then
                for name <- names do
                  name match {
                    case Left(name) =>
                      changingProperties.getOrElseUpdate(name, ListBuffer()) += { () =>
                        if getter() != value.get then
                          Swing onEDT { if getter() != value.get then set(value.get) }
                      }

                    case Right((publisher, reaction)) =>
                      reactor listenTo publisher
                      changingReactions.getOrElseUpdate(reaction, ListBuffer()) += { () =>
                        if getter() != value.get then
                          Swing onEDT { if getter() != value.get then set(value.get) }
                      }
                  }
            }

            if !value.fixed then {
              value() = getter()
              for name <- names do
                name match {
                  case Left(name) =>
                    changingProperties.getOrElseUpdate(name, ListBuffer()) += { () =>
                      updatingSwingNotification = true
                      value() = getter()
                      updatingSwingNotification = false
                    }

                  case Right((publisher, reaction)) =>
                    reactor listenTo publisher
                    changingReactions.getOrElseUpdate(reaction, ListBuffer()) += { () =>
                      updatingSwingNotification = true
                      value() = getter()
                      updatingSwingNotification = false
                    }
                }
            }
          }
          ()
        }
      value
    }

    /** Forces the property given by `name` the keep the given value using the
      * given setter, if the [[ReSwingValue]] is set to hold a fixed value,
      * i.e. a value that should not be changed by the library.
      */
    def force[U](name: String, setter: U => Unit, forcedValue: U): ReSwingValue[T] = {
      if value != null && value.fixed then
        inSyncEDT {
          setter(forcedValue)
          enforcedProperties += name -> { () => Swing onEDT { setter(forcedValue) } }
          ()
        }
      value
    }
  }

  private val delayedInitValues: ListBuffer[() => Unit] = ListBuffer.empty[() => Unit]
  private val changingReactions                         = Map.empty[Class[?], ListBuffer[() => Unit]]
  private val changingProperties                        = Map.empty[String, ListBuffer[() => Unit]]
  private val enforcedProperties                        = Map.empty[String, () => Unit]

  private val reactor: Reactor = new Reactor {
    reactions += {
      case e: Event =>
        for signals <- changingReactions get e.getClass; signal <- signals do
          signal()
    }
  }

  peer.peer.addPropertyChangeListener(new java.beans.PropertyChangeListener {
    def propertyChange(e: java.beans.PropertyChangeEvent): Unit = {
      enforcedProperties get e.getPropertyName match {
        case Some(setter) => setter()
        case _ => changingProperties get e.getPropertyName match {
            case Some(signals) => for signal <- signals do signal()
            case _             =>
          }
      }
    }
  })

  peer.peer.addHierarchyListener(new HierarchyListener {
    def hierarchyChanged(e: HierarchyEvent): Unit =
      if (e.getChangeFlags & HierarchyEvent.DISPLAYABILITY_CHANGED) != 0 then {
        initReSwingValueConnection()
        peer.peer.removeHierarchyListener(this)
      }
  })

  protected def initReSwingValueConnection(): Unit = {
    for init <- delayedInitValues do
      init()
    delayedInitValues.clear()
  }
}
