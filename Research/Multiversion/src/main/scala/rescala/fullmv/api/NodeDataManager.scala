package rescala.fullmv.api

trait NodeDataManager[V, O] extends Iterable[(Transaction, V, O)] {
  /**
    * prepare for writing v
    * @param txn must be preparing
    * @return outgoing dependencies for continuing framing traversal
    */
  def incrementFrame(txn: Transaction): Set[O]
  /**
    * count glitch-freedom with changed notification
    * @param txn must be executing, just having completed reev_out(n) changed for some n->this
    */
  def changed(txn: Transaction): Unit
  /**
    * count glitch-freedom with unchanged notification
    * @param txn must be executing, just having completed reev_out(n) unchanged for some n->this
    */
  def unchanged(txn: Transaction): Unit
  /**
    * convert one previous changed notification into an unchanged notification
    * @param txn must be executing, just having dropped some edge n->this
    */
  def dechanged(txn: Transaction): Unit
  /**
    * write v
    * @param txn must be executing and have framed but not written before
    * @param value the value to write
    * @return outgoing dependencies for continuing propagation traversal
    */
  def reevOut(txn: Transaction, value: V): Set[O]

  /**
    * read glitch-free v along dependency edge without suspending
    * @param txn must be reevaluating d of some edge this->d
    * @return preceding v if no own frame/version, otherwise written own v (glitch-free)
    */
  def depRead(txn: Transaction): V
  /**
    * read old v from before txn
    * @param txn must be executing
    * @return preceding v, ignoring own frame/version
    */
  def old(txn: Transaction): V
  /**
    * read current v
    * @param txn must be executing
    * @return own v if written, otherwise preceding v
    */
  def now(txn: Transaction): V
  /**
    * read glitch-free v with suspending
    * @param txn must be executing
    * @return preceding v if no own frame/version, otherwise written own v (glitch-free)
    */
  def after(txn: Transaction): V

  /**
    * discover dependency (add outgoing dependency and read glitch-free v with suspending)
    * @param txn must be reevaluating add
    * @param add the newly discovered dependency edge's sink
    * @return preceding v if no own frame/version, otherwise written own v
    */
  def discover(txn: Transaction, add: O): V
  /**
    * drop dependency (remove outgoing dependency)
    * @param txn must be reevaluating remove
    * @param remove the now obsolete dependency edge's sink
    */
  def drop(txn: Transaction, remove: O): Unit

  /**
    * remove obsolete versions
    */
  def expunge(): Unit
}

object NodeDataManager {
  def apply[T, O](sgt: SerializationGraphTracking, creator: Transaction, initialValue: T): NodeDataManager[T, O] = ???
}
