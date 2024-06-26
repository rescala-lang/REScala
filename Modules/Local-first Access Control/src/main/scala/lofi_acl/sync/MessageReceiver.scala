package lofi_acl.sync

import lofi_acl.crypto.PublicIdentity

trait MessageReceiver[MSG]:
  def receivedMessage(msg: MSG, fromUser: PublicIdentity): Unit
  def connectionEstablished(publicIdentity: PublicIdentity): Unit = {}
  def connectionShutdown(publicIdentity: PublicIdentity): Unit    = {}
