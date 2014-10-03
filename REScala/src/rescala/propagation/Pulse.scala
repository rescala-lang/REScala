package rescala.propagation

sealed trait Pulse[+P] {
  def toOption: Option[P]
  def isChange: Boolean
  def map[Q](f: P => Q): Pulse[Q]
  def flatMap[Q](f: P => Pulse[Q]): Pulse[Q]
  def filter(p: P => Boolean): Pulse[P]
}

object Pulse {
  def fromOption[P](opt: Option[P]): Pulse[P] = opt.fold[Pulse[P]](NoChange())(Diff(_))

  def change[P](value: P) = Diff(value)

  def unchanged[P](value: P) = NoChange(Some(value))
  
  def diff[P](newValue: P, oldValue: P) =
    if (null == oldValue) change(newValue)
    else if (newValue == oldValue)  unchanged(newValue)
    else Diff(newValue, Some(oldValue))

  val none = NoChange()

  case class NoChange[+P](current: Option[P] = None) extends Pulse[P] {
    override def toOption: Option[P] = None
    override def isChange: Boolean = false
    override def map[Q](f: (P) => Q): Pulse[Q] = none
    override def flatMap[Q](f: (P) => Pulse[Q]): Pulse[Q] = none
    override def filter(p: (P) => Boolean): Pulse[P] = none
  }

  case class Diff[+P](update: P, current: Option[P] = None) extends Pulse[P] {
    override def toOption: Option[P] = Some(update)
    override def isChange: Boolean = true
    override def map[Q](f: (P) => Q): Pulse[Q] = change(f(update))
    override def flatMap[Q](f: (P) => Pulse[Q]): Pulse[Q] = f(update)
    override def filter(p: (P) => Boolean): Pulse[P] = if (p(update)) change(update) else none
  }
}
