package rescala.propagation

sealed trait EvaluationResult

object EvaluationResult {
  case class Done(changed: Boolean, changedDependencies: Option[DependencyDiff] = None) extends EvaluationResult
  case class DependencyDiff(newDependencies: Set[Reactive], oldDependencies: Set[Reactive]) extends EvaluationResult
}


sealed trait Pulse[+P] {
  def toOption: Option[P]
  def isChange: Boolean
  def map[Q](f: P => Q): Pulse[Q]
  def flatMap[Q](f: P => Pulse[Q]): Pulse[Q]
  def filter(p: P => Boolean): Pulse[P]
  def fold[Q](ifNone: => Q)(ifChange: P => Q): Q
  def keep: Pulse[P]
  def current: Option[P]
}

object Pulse {
  def fromOption[P](opt: Option[P]): Pulse[P] = opt.fold[Pulse[P]](NoChange())(Diff(_))

  def change[P](value: P): Pulse[P] = Diff(value)

  def unchanged[P](value: P): Pulse[P] = NoChange(Some(value))
  
  def diff[P](newValue: P, oldValue: P): Pulse[P] =
    if (null == oldValue) change(newValue)
    else if (newValue == oldValue)  unchanged(newValue)
    else Diff(newValue, Some(oldValue))

  def diffPulse[P](newValue: P, oldPulse: Pulse[P]): Pulse[P] = oldPulse match {
    case NoChange(None) => change(newValue)
    case NoChange(Some(value)) => diff(newValue, value)
    case Diff(update, current) => diff(newValue, update)
  }

  val none: Pulse[Nothing] = NoChange()

  final case class NoChange[+P](current: Option[P] = None) extends Pulse[P] {
    override def toOption: Option[P] = None
    override def isChange: Boolean = false
    override def map[Q](f: (P) => Q): Pulse[Q] = none
    override def flatMap[Q](f: (P) => Pulse[Q]): Pulse[Q] = none
    override def filter(p: (P) => Boolean): Pulse[P] = none
    override def fold[Q](ifNone: => Q)(ifChange: (P) => Q): Q = ifNone
    override def keep: Pulse[P] = this
  }

  final case class Diff[+P](update: P, current: Option[P] = None) extends Pulse[P] {
    override def toOption: Option[P] = Some(update)
    override def isChange: Boolean = true
    override def map[Q](f: (P) => Q): Pulse[Q] = change(f(update))
    override def flatMap[Q](f: (P) => Pulse[Q]): Pulse[Q] = f(update)
    override def filter(p: (P) => Boolean): Pulse[P] = if (p(update)) change(update) else none
    override def fold[Q](ifNone: => Q)(ifChange: (P) => Q): Q = ifChange(update)
    override def keep: Pulse[P] = unchanged(update)
  }
}
