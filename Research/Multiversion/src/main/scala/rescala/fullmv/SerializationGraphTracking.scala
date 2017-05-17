package rescala.fullmv

sealed trait PartialOrderResult
case object Unordered extends PartialOrderResult
sealed trait OrderResult extends PartialOrderResult
case object FirstFirst extends OrderResult
case object SecondFirst extends OrderResult


trait SerializationGraphTracking[T] {
  /**
    * query for existing order between transactions. Must not be called
    * with equal transactions in both parameters!
    * @param a first transaction
    * @param b second transaction
    * @return the previously established order or [[Unordered]]
    */
  def getOrder(a: T, b: T): PartialOrderResult

  /**
    * require and query for an order between the given transactions.
    * Must not be called with equal transactions in both parameters!
    * Note that the parameters in this method are order sensitive: If
    * [[getOrder]] would return [[Unordered]], [[FirstFirst]] will
    * be established and returned, ensuring that newly arriving
    * transactions perform their operations AFTER those that have
    * acted there in the past. Reversing the parameters in a call to
    * this method will negate linearizability (wall-clock ordering).
    * @param defender the transaction already in place at a given variable
    * @param contender the transaction newly arriving at that variable
    * @return the established order
    */
  def ensureOrder(defender: T, contender: T): OrderResult

  def awaitAllPredecessorsState(turn: T, atLeast: State.Type): Unit
}

trait IFullMVTurn {
  def state: State.type
}

object State {
  type Type = Int
  val Initialized: Type = 0
  val Framing: Type = 1
  val Executing: Type = 2
  val WrapUp: Type = 3
  val Completed: Type = 4
}
