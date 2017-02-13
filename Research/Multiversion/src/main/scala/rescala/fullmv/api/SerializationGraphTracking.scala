package rescala.fullmv.api

sealed trait OrderResult
case object FirstFirst extends OrderResult
case object SecondFirst extends OrderResult

trait SerializationGraphTracking {
  def getOrder(a: Transaction, b: Transaction): Option[OrderResult]
  def requireOrder(defender: Transaction, contender: Transaction): OrderResult
}

object SerializationGraphTracking {
  def apply(): SerializationGraphTracking = ???
}

