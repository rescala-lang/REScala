package rescala.propagation

sealed trait Pulse[+P] {
  def toOption: Option[P]
  def isChange: Boolean
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

  case class NoChange[+P](currentValue: Option[P] = None) extends Pulse[P] {
    override def toOption: Option[P] = None
    override def isChange: Boolean = false
  }

  case class Diff[+P](value: P, oldOption: Option[P] = None) extends Pulse[P] {
    override def toOption: Option[P] = Some(value)
    override def isChange: Boolean = true
  }
}
