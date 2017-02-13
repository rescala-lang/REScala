package rescala.fullmv.api

sealed trait PartialOrderResult
case object Unordered extends PartialOrderResult
sealed trait OrderResult extends PartialOrderResult
case object FirstFirst extends OrderResult
case object SecondFirst extends OrderResult


trait SerializationGraphTracking {
  /**
    * query for existing order between transactions. Must not be called
    * with equal transactions in both parameters!
    * @param a first transaction
    * @param b second transaction
    * @return the previously established order or [[Unordered]]
    */
  def getOrder(a: Transaction, b: Transaction): PartialOrderResult

  /**
    * require and query for an order between the given transactions.
    * Must not be called with equal transactions in both parameters!
    * Note that the parameters in this method are order sensitive: If
    * [[getOrder()]] would return [[Unordered]], [[FirstFirst]] will
    * be established and returned, ensuring that newly arriving
    * transactions perform their operations AFTER those that have
    * acted there in the past. Reversing the parameters in a call to
    * this method will negate linearizability (wall-clock ordering).
    * @param defender the transaction already in place at a given variable
    * @param contender the transaction newly arriving at that variable
    * @return the established order
    */
  def ensureOrder(defender: Transaction, contender: Transaction): OrderResult
}

object SerializationGraphTracking {
  def apply(): SerializationGraphTracking = ???
}

object GlobalLockSGT extends SerializationGraphTracking {
  var predecessors = Map[Transaction, Set[Transaction]]().withDefaultValue(Set.empty)
  var successors = Map[Transaction, Set[Transaction]]().withDefaultValue(Set.empty)
  override def getOrder(a: Transaction, b: Transaction): PartialOrderResult = synchronized {
    if(successors(a).contains(b)) {
      FirstFirst
    } else if (predecessors(a).contains(b)) {
      SecondFirst
    } else {
      Unordered
    }
  }
  override def ensureOrder(defender: Transaction, contender: Transaction): OrderResult = synchronized {
    if(successors(defender).contains(contender)) {
      FirstFirst
    } else if (predecessors(defender).contains(contender)) {
      SecondFirst
    } else {
      val newSuccessors = successors(contender) + contender
      val newPredecessors = predecessors(defender) + defender
      for(successor <- newSuccessors) {
        predecessors += successor -> (predecessors(successor) ++ newPredecessors)
      }
      for(predecessor <- newPredecessors) {
        successors += predecessor -> (successors(predecessor) ++ newSuccessors)
      }
      FirstFirst
    }
  }
}
