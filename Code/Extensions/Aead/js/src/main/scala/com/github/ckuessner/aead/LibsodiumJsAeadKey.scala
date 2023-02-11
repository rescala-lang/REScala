package com.github.ckuessner.aead

import typings.libsodiumWrappers.mod as sodium

import scala.scalajs.js.typedarray.Uint8Array

case class LibsodiumJsAeadKey(rawKeyBytes: Uint8Array) extends AeadKey {
  override def aeadPrimitive: Aead = LibsodiumJsAead(rawKeyBytes)

  override def rawKeyBase64: String = sodium.toBase64(rawKeyBytes, sodium.base64Variants.ORIGINAL)
}
