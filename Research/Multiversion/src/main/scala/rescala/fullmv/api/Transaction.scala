package rescala.fullmv.api

sealed trait Phase
object Preparing extends Phase
object Running extends Phase
object Completed extends Phase
object Obsolete extends Phase

trait Transaction {
  def phase: Phase
  def start(): this.type
  def done(): this.type
}

object Transaction {
  def apply(): Transaction = ???
}
