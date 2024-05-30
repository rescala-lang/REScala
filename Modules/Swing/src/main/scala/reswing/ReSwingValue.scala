package reswing

import reactives.default.*

/** Combines reactive values from the application and from the `Swing` library */
sealed abstract class ReSwingValue[T] {
  protected def signal: Lazy[Signal[T]]
  protected val event: Lazy[Evt[T]] = Lazy { Evt[T]() }
  protected var latestValue         = null.asInstanceOf[T]

  private var init = null: (ReSwingValue[T] => Unit)

  final protected def toSignal: Signal[T] = {
    initPerform()
    signal()
  }

  private[reswing] def fixed: Boolean
  private[reswing] def get: T
  private[reswing] def use(setter: T => Unit): Unit

  final private[reswing] def update(value: T): Unit = {
    latestValue = value; if event.isDefined then event().fire(value)
  }
  final private[reswing] def initLazily(initLazily: ReSwingValue[T] => Unit): Unit = {
    if signal.isDefined then initLazily(this) else init = initLazily
  }
  final private[reswing] def initPerform(): Unit = { if init != null then { init(this); init = null } }
}

final case class ReSwingNoValue[T]() extends ReSwingValue[T] {
  protected val signal                              = Lazy { event().hold(latestValue) }
  private[reswing] def fixed                        = false
  private[reswing] def get                          = latestValue
  private[reswing] def use(setter: T => Unit): Unit = {}
}

final case class ReSwingValueValue[T](private val value: T) extends ReSwingValue[T] {
  protected val signal                        = Lazy { event() `hold` latestValue }
  private[reswing] def fixed                  = false
  private[reswing] def get                    = latestValue
  private[reswing] def use(setter: T => Unit) = setter(value)
}

final case class ReSwingEventValue[T](private val value: Lazy[Event[T]]) extends ReSwingValue[T] {
  protected val signal       = Lazy { (value() || event()) `hold` latestValue }
  private[reswing] def fixed = false
  private[reswing] def get   = latestValue
  private[reswing] def use(setter: T => Unit) = {
    value() observe { value => if get != value then setter(value) }
    ()
  }
}

final case class ReSwingSignalValue[T](private val value: Lazy[Signal[T]]) extends ReSwingValue[T] {
  protected val signal       = Lazy { (value().changed || event()) `hold` value().readValueOnce }
  private[reswing] def fixed = true
  private[reswing] def get   = value().readValueOnce
  private[reswing] def use(setter: T => Unit): Unit = {
    value().changed `observe` setter; setter(value().readValueOnce)
  }
}

object ReSwingValue {

  /** Does not cause the `Swing` library to use a specific value. */
  implicit def apply[T](value: Unit): ReSwingNoValue[T] = ReSwingNoValue[T]()

  /** Sets the given value once.
    * After this, does not cause the `Swing` library to use a specific value.
    */
  implicit def apply[T](value: T): ReSwingValueValue[T] = ReSwingValueValue(value)

  /** Sets the value whenever the given Event changes. */
  implicit def apply[T](value: => Event[T]): ReSwingEventValue[T] = ReSwingEventValue(Lazy { value })

  /** Sets the value to the value of the given Signal and causes
    * the `Swing` library to always use the current `Signal` value.
    */
  implicit def apply[T](value: => Signal[T]): ReSwingSignalValue[T] = ReSwingSignalValue(Lazy { value })

  /** Returns the Signal representing the value. */
  implicit def toSignal[T](value: ReSwingValue[T]): Signal[T] = value.toSignal
}
