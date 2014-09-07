package rescala.propagation

sealed trait Pulse[+P] {
  def valueOption: Option[P]
  def oldOption: Option[P]
}

object Pulse {
  def apply[P](opt: Option[P]): Pulse[P] = opt.fold[Pulse[P]](NoChangePulse)(ValuePulse(_))
}

case object NoChangePulse extends Pulse[Nothing] {
  override def valueOption: Option[Nothing] = None
  override def oldOption: Option[Nothing] = None
}
case class ValuePulse[+P](value: P) extends Pulse[P] {
  override def valueOption: Option[P] = Some(value)
  override def oldOption: Option[P] = None
}
case class DiffPulse[+P](value: P, old: P) extends Pulse[P] {
  override def valueOption: Option[P] = Some(value)
  override def oldOption: Option[P] = Some(old)
}
