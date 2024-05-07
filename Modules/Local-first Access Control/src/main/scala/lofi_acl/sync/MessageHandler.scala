package lofi_acl.sync

import lofi_acl.crypto.PublicIdentity

trait MessageHandler[MSG]:
  def receivedMessage(msg: MSG, fromUser: PublicIdentity): Unit
