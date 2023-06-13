package com.github.ckuessner.aead

import com.google.crypto.tink.subtle.{Base64, XChaCha20Poly1305}

case class TinkAeadKey(rawKeyBytes: Array[Byte]) extends AeadKey {
  override def aeadPrimitive: TinkAead = TinkAead(new XChaCha20Poly1305(rawKeyBytes))

  def rawKeyBase64: String = Base64.encode(rawKeyBytes)
}
