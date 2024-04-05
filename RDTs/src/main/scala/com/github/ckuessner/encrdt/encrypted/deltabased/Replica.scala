package com.github.ckuessner.encrdt.encrypted.deltabased

trait Replica {
  def receive(encryptedState: EncryptedDeltaGroup): Unit

  protected def disseminate(encryptedState: EncryptedDeltaGroup): Unit
}
