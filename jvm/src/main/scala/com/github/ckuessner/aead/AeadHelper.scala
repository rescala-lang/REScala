package com.github.ckuessner.aead

import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.subtle.Base64
import com.google.crypto.tink.{KeyTemplates, KeysetHandle, Registry}

import java.security.GeneralSecurityException
import scala.concurrent.Future

object AeadHelper {

  /** Doesn't do anything, only here for compatibility with JS version.
    *
    * @return
    *   A completed Future
    */
  def ready(): Future[Unit] = Future.unit

  def toBase64(bytes: Array[Byte]): String = Base64.encode(bytes)

  def fromBase64(bytes: String): Array[Byte]= Base64.decode(bytes)

  def generateKey: TinkAeadKey = TinkAeadKey(
    com.google.crypto.tink.subtle.Random.randBytes(32)
  )

  def aeadKeyFromBytes(key: Array[Byte]): TinkAeadKey = {
    TinkAeadKey(key)
  }

  def aeadKeyFromBase64(base64EncodedString: String): TinkAeadKey = {
    TinkAeadKey(Base64.decode(base64EncodedString))
  }
}
