package lofi_acl.encrdt

trait Replica {
  def receive(encryptedState: EncryptedDeltaGroup): Unit

  protected def disseminate(encryptedState: EncryptedDeltaGroup): Unit
}
