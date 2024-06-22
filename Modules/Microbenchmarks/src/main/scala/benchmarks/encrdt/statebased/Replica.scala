package benchmarks.encrdt.statebased

trait Replica {
  def receive(encryptedState: EncryptedState): Unit

  protected def disseminate(encryptedState: EncryptedState): Unit
}
