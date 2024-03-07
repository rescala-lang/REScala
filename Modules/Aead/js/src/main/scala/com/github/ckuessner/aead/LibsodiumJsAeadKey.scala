package com.github.ckuessner.aead

import scala.scalajs.js.typedarray.Uint8Array

case class LibsodiumJsAeadKey(rawKeyBytes: Uint8Array) extends AeadKey {
  override def aeadPrimitive: Aead = LibsodiumJsAead(rawKeyBytes)

  override def rawKeyBase64: String = sodium.to_base64(rawKeyBytes, base64Variants.ORIGINAL)
}
