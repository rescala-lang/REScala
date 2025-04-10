package lofi_acl.sync

import crypto.PublicIdentity

trait MessageReceiver[MSG]:
  def receivedMessage(msg: MSG, fromUser: PublicIdentity): Unit
  def connectionEstablished(publicIdentity: PublicIdentity): Unit = {}
  def connectionShutdown(publicIdentity: PublicIdentity): Unit    = {}
