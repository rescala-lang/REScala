package rescala.fullmv.api

sealed trait OrderResult
case object FirstFirst extends OrderResult
case object SecondFirst extends OrderResult

trait SerializationGraphTracking {
  def getOrder(a: Transaction, b: Transaction): Option[OrderResult]
  def requireOrder(defender: Transaction, contender: Transaction): OrderResult

  val fairSearchOrdering = new Ordering[Transaction] {
    override def compare(x: Transaction, y: Transaction): Int = {
      if (x == y){
        0
      } else {
        getOrder(x, y) match {
          case Some(FirstFirst) => -1
          case Some(SecondFirst) => 1
          case None => -1
        }
      }
    }
  }
}

object SerializationGraphTracking {
  def apply(): SerializationGraphTracking = ???
}

