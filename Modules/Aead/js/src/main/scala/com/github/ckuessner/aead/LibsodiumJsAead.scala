package com.github.ckuessner.aead

import scala.scalajs.js.typedarray.Uint8Array
import scala.util.Try

class LibsodiumJsAead(private val key: Uint8Array) extends Aead {
  override def encrypt(plaintext: Uint8Array, associatedData: Uint8Array): Try[Uint8Array] = {
    AeadHelper.encrypt(plaintext, associatedData, key)
  }

  override def encrypt(plaintext: String, associatedData: String): Try[Uint8Array] = {
    AeadHelper.encrypt(plaintext, associatedData, key)
  }

  override def decrypt(ciphertext: Uint8Array, associatedData: Uint8Array): Try[Uint8Array] = {
    AeadHelper.decrypt(ciphertext, associatedData, key)
  }

  override def decrypt(ciphertext: Uint8Array, associatedData: String): Try[String] = {
    AeadHelper.decrypt(ciphertext, associatedData, key)
  }
}
