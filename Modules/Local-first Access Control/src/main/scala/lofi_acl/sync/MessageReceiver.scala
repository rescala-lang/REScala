package lofi_acl.sync

import lofi_acl.crypto.PublicIdentity

trait MessageReceiver[MSG]:
  def receivedMessage(msg: MSG, fromUser: PublicIdentity): Unit
