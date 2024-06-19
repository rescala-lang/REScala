package lofi_acl.sync.acl.monotonic

import lofi_acl.access.Filter
import lofi_acl.crypto.PublicIdentity
import lofi_acl.sync.MessageReceiver
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.Delta

/** Filters the received messages using the supplied ACL.
  *
  * Used by FilteringConnectionManager to filter received deltas.
  *
  * @param delegate The actual message handler
  * @param acl The acl used for filtering
  * @param filter The filter instance
  * @tparam RDT The type of the filtered deltas
  */
class FilteringMessageReceiver[RDT](
    private val delegate: MessageReceiver[MonotonicAclSyncMessage[RDT]],
    private val acl: MonotonicAcl[RDT]
)(using filter: Filter[RDT])
    extends MessageReceiver[MonotonicAclSyncMessage[RDT]] {

  override def receivedMessage(msg: MonotonicAclSyncMessage[RDT], fromUser: PublicIdentity): Unit =
    msg match
      case deltaMsg @ Delta(delta, _, _) =>
        val filteredDelta = acl.filterReceivedDelta(delta, fromUser)
        delegate.receivedMessage(deltaMsg.copy(delta = filteredDelta), fromUser)
      case _ => delegate.receivedMessage(msg, fromUser)
}
