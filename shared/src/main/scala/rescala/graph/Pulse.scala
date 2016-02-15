package rescala.graph

import java.util.concurrent.ThreadLocalRandom

import scala.util.DynamicVariable





sealed trait Pulse[+P] {

  import rescala.graph.Pulse._

  def fold[Q](ifNone: => Q, ifChange: P => Q): Q
  def current: Option[P]
  final def toOption: Option[P] = fold(None, Some.apply)
  final def isChange: Boolean = fold(false, _ => true)
  final def map[Q](f: P => Q): Pulse[Q] = fold(none, f.andThen(change))
  final def flatMap[Q](f: P => Pulse[Q]): Pulse[Q] = fold(none, f)
  final def filter(p: P => Boolean): Pulse[P] = fold(none, up => if (p(up)) change(up) else none)
  final def keep: Pulse[P] = fold(this, unchanged)
}

object Pulse {
  def fromOption[P](opt: Option[P]): Pulse[P] = opt.fold[Pulse[P]](NoChange())(Diff(_))

  def change[P](value: P): Pulse[P] = Diff(value)

  def unchanged[P](value: P): Pulse[P] = NoChange(Some(value))

  def diff[P](newValue: P, oldValue: P): Pulse[P] =
    if (null == oldValue) change(newValue)
    else if (newValue == oldValue) unchanged(oldValue)
    else Diff(newValue, Some(oldValue))

  def diffPulse[P](newValue: P, oldPulse: Pulse[P]): Pulse[P] = oldPulse match {
    case NoChange(None) => change(newValue)
    case NoChange(Some(value)) => diff(newValue, value)
    case Diff(update, current) => diff(newValue, update)
  }

  val none: Pulse[Nothing] = NoChange()

  final case class NoChange[+P](override val current: Option[P] = None) extends Pulse[P] {
    override def fold[Q](ifNone: => Q, ifChange: (P) => Q): Q = ifNone
  }
  final case class Diff[+P](update: P, override val current: Option[P] = None) extends Pulse[P] {
    override def fold[Q](ifNone: => Q, ifChange: (P) => Q): Q = ifChange(update)
  }
}
