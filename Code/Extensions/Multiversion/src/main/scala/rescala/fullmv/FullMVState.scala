package rescala.fullmv

trait FullMVState[V, T <: FullMVTurn, Reactive, OutDep] {
  val host: FullMVEngine

  var incomings: Set[Reactive] = Set.empty

  def latestValue: V

  /** entry point for regular framing
    *
    * @param txn the transaction visiting the node for framing
    */
  def incrementFrame(txn: T): FramingBranchResult[T, OutDep]

  /** entry point for superseding framing
    *
    * @param txn       the transaction visiting the node for framing
    * @param supersede the transaction whose frame was superseded by the visiting transaction at the previous node
    */
  def incrementSupersedeFrame(txn: T, supersede: T): FramingBranchResult[T, OutDep]

  /** entry point for change/nochange notification reception
    *
    * @param txn     the transaction sending the notification
    * @param changed whether or not the dependency changed
    */
  def notify(txn: T, changed: Boolean): (Boolean, NotificationBranchResult[T, OutDep])

  /** entry point for change/nochange notification reception with follow-up framing
    *
    * @param txn         the transaction sending the notification
    * @param changed     whether or not the dependency changed
    * @param followFrame a transaction for which to create a subsequent frame, furthering its partial framing.
    */
  def notifyFollowFrame(txn: T, changed: Boolean, followFrame: T): (Boolean, NotificationBranchResult[T, OutDep])

  def reevIn(turn: T): V

  /** progress [[firstFrame]] forward until a [[Version.isFrame]] is encountered, and
    * return the resulting notification out (with reframing if subsequent write is found).
    */
  def reevOut(turn: T, maybeValue: Option[V], unchange: V => V): NotificationBranchResult.ReevOutBranchResult[T, OutDep]

  /** entry point for before(this); may suspend.
    *
    * @param txn the executing transaction
    * @return the corresponding [[Version.value]] from before this transaction, i.e., ignoring the transaction's
    *         own writes.
    */
  def dynamicBefore(txn: T): V

  def staticBefore(txn: T): V

  /** entry point for after(this); may suspend.
    *
    * @param txn the executing transaction
    * @return the corresponding [[Version.value]] from after this transaction, i.e., awaiting and returning the
    *         transaction's own write if one has occurred or will occur.
    */
  def dynamicAfter(txn: T): V

  def staticAfter(txn: T): V

  /** entry point for discover(this, add). May suspend.
    *
    * @param txn the executing reevaluation's transaction
    * @param add the new edge's sink node
    * @return the appropriate [[Version.value]].
    */
  def discover(txn: T, add: OutDep): (List[T], Option[T])

  /** entry point for drop(this, ticket.issuer); may suspend temporarily.
    *
    * @param txn    the executing reevaluation's transaction
    * @param remove the removed edge's sink node
    */
  def drop(txn: T, remove: OutDep): (List[T], Option[T])

  /** performs the reframings on the sink of a discover(n, this) with arity +1, or drop(n, this) with arity -1
    *
    * @param successorWrittenVersions the reframings to perform for successor written versions
    * @param maybeSuccessorFrame      maybe a reframing to perform for the first successor frame
    * @param arity                    +1 for discover adding frames, -1 for drop removing frames.
    */
  def retrofitSinkFrames(successorWrittenVersions: Seq[T], maybeSuccessorFrame: Option[T], arity: Int): Seq[T]
}
