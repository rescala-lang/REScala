package lofi_acl.sync.acl.monotonic

import lofi_acl.access.Filter
import lofi_acl.crypto.{PrivateIdentity, PublicIdentity}
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.*
import lofi_acl.sync.{ConnectionManager, MessageReceiver, MessageSerialization}
import rdts.time.Dots

class FilteringConnectionManager[RDT](
    privateIdentity: PrivateIdentity,
    messageHandler: MessageReceiver[MonotonicAclSyncMessage[RDT]],
    acl: MonotonicAcl[RDT],
    val aclVersion: Dots
)(using msgCodec: MessageSerialization[MonotonicAclSyncMessage[RDT]], filter: Filter[RDT])
    extends ConnectionManager[MonotonicAclSyncMessage[RDT]](privateIdentity, messageHandler)(using msgCodec) {

  override def sendMultiple(receivingUser: PublicIdentity, msgs: MonotonicAclSyncMessage[RDT]*): Boolean =
    val filteredMsgs = msgs.map {
      case deltaMsg @ Delta(delta, _, _) => deltaMsg.copy(delta = acl.filterDeltaToSend(delta, receivingUser))
      case msg                           => msg
    }
    super.sendMultiple(receivingUser, filteredMsgs*)
}
