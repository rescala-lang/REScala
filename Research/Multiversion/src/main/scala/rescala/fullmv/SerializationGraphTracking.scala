package rescala.fullmv

import scala.concurrent.duration.Duration

sealed trait PartialOrderResult
case object UnorderedSCCUnknown extends PartialOrderResult
case object FirstFirstSCCUnkown extends PartialOrderResult
case object FirstFirstSameSCC extends PartialOrderResult
case object SecondFirstSameSCC extends PartialOrderResult

sealed trait OrderResult
case object FirstFirst extends OrderResult
case object SecondFirst extends OrderResult

trait SerializationGraphTracking[T] {
  /**
    * query for existing order between transactions. Must not be called
    * with equal transactions in both parameters!
 *
    * @param found a transaction already in place at a given variable
    * @param searcher the transaction newly arriving at that variable
    * @return the previously established order or [[UnorderedSCCUnknown]]
    */
  def getOrder(found: T, searcher: T): PartialOrderResult

  /**
    * require and query for an order between the given transactions.
    * Must not be called with equal transactions in both parameters!
    * Note that the parameters in this method are order sensitive: If
    * [[getOrder]] would return [[UnorderedSCCUnknown]], [[FirstFirst]] will
    * be established and returned, ensuring that newly arriving
    * transactions perform their operations AFTER those that have
    * acted there in the past. Reversing the parameters in a call to
    * this method will negate linearizability (wall-clock ordering).
 *
    * @param defender the transaction already in place at a given variable
    * @param contender the transaction newly arriving at that variable
    * @return the established order
    */
  def ensureOrder(defender: T, contender: T, timeout: Duration): OrderResult
}
