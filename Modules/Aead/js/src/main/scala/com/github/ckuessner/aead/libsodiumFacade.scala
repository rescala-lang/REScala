package com.github.ckuessner.aead

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSBracketAccess, JSGlobal, JSGlobalScope, JSImport, JSName}
import scala.scalajs.js.typedarray.Uint8Array

object libsodiumWrappersStrings {

  @js.native
  sealed trait base64
      extends js.Object
      with StringOutputFormat
  def base64: base64 = "base64".asInstanceOf[base64]

  @js.native
  sealed trait curve25519
      extends js.Object
      with KeyType
  def curve25519: curve25519 = "curve25519".asInstanceOf[curve25519]

  @js.native
  sealed trait ed25519
      extends js.Object
      with KeyType
  def ed25519: ed25519 = "ed25519".asInstanceOf[ed25519]

  @js.native
  sealed trait hex
      extends js.Object
      with StringOutputFormat
  def hex: hex = "hex".asInstanceOf[hex]

  @js.native
  sealed trait text
      extends js.Object
      with StringOutputFormat
  def text: text = "text".asInstanceOf[text]

  @js.native
  sealed trait uint8array extends js.Object
  def uint8array: uint8array = "uint8array".asInstanceOf[uint8array]

  @js.native
  sealed trait x25519
      extends js.Object
      with KeyType
  def x25519: x25519 = "x25519".asInstanceOf[x25519]
}

trait KeyType extends js.Object
object KeyType {

  def curve25519: libsodiumWrappersStrings.curve25519 =
    "curve25519".asInstanceOf[libsodiumWrappersStrings.curve25519]

  def ed25519: libsodiumWrappersStrings.ed25519 =
    "ed25519".asInstanceOf[libsodiumWrappersStrings.ed25519]

  def x25519: libsodiumWrappersStrings.x25519 =
    "x25519".asInstanceOf[libsodiumWrappersStrings.x25519]
}

trait StringOutputFormat extends js.Object
object StringOutputFormat {

  def base64: libsodiumWrappersStrings.base64 =
    "base64".asInstanceOf[libsodiumWrappersStrings.base64]

  def hex: libsodiumWrappersStrings.hex =
    "hex".asInstanceOf[libsodiumWrappersStrings.hex]

  def text: libsodiumWrappersStrings.text =
    "text".asInstanceOf[libsodiumWrappersStrings.text]
}

@js.native
sealed trait base64Variants extends js.Object

@JSImport("libsodium-wrappers", "base64_variants")
@js.native
object base64Variants extends js.Object {

  @JSBracketAccess
  def apply(value: Double): js.UndefOr[base64Variants & Double] = js.native

  @js.native
  sealed trait ORIGINAL
      extends js.Object
      with base64Variants
  /* 0 */
  val ORIGINAL: base64Variants.ORIGINAL & Double = js.native

  @js.native
  sealed trait ORIGINAL_NO_PADDING
      extends js.Object
      with base64Variants
  /* 1 */
  val ORIGINAL_NO_PADDING: base64Variants.ORIGINAL_NO_PADDING & Double = js.native

  @js.native
  sealed trait URLSAFE
      extends js.Object
      with base64Variants
  /* 2 */
  val URLSAFE: base64Variants.URLSAFE & Double = js.native

  @js.native
  sealed trait URLSAFE_NO_PADDING
      extends js.Object
      with base64Variants
  /* 3 */
  val URLSAFE_NO_PADDING: base64Variants.URLSAFE_NO_PADDING & Double = js.native
}

@JSImport("libsodium-wrappers", JSImport.Namespace)
@js.native
object sodium extends js.Object {

  def SODIUM_LIBRARY_VERSION_MAJOR: _root_.scala.Double = js.native

  def SODIUM_LIBRARY_VERSION_MINOR: _root_.scala.Double = js.native

  def SODIUM_VERSION_STRING: _root_.scala.Predef.String = js.native

  def add(
      a: _root_.scala.scalajs.js.typedarray.Uint8Array,
      b: _root_.scala.scalajs.js.typedarray.Uint8Array
  ): _root_.scala.Unit = js.native

  def crypto_aead_xchacha20poly1305_ietf_ABYTES: _root_.scala.Double = js.native

  def crypto_aead_xchacha20poly1305_ietf_decrypt(
      secret_nonce: Uint8Array | Null,
      ciphertext: Uint8Array,
      additional_data: String | Null,
      public_nonce: Uint8Array,
      key: Uint8Array,
      outputFormat: js.UndefOr[StringOutputFormat | Null]
  ): String = js.native

  def crypto_aead_xchacha20poly1305_ietf_decrypt(
      secret_nonce: Uint8Array | Null,
      ciphertext: Uint8Array,
      additional_data: Uint8Array | Null,
      public_nonce: Uint8Array,
      key: Uint8Array,
      outputFormat: js.UndefOr[StringOutputFormat | Null]
  ): Uint8Array = js.native

  def crypto_aead_xchacha20poly1305_ietf_encrypt(
      message: _root_.scala.|[_root_.scala.Predef.String, _root_.scala.scalajs.js.typedarray.Uint8Array],
      additional_data: _root_.scala.|[
        _root_.scala.|[_root_.scala.Predef.String, _root_.scala.scalajs.js.typedarray.Uint8Array],
        _root_.scala.Null
      ],
      secret_nonce: _root_.scala.|[
        _root_.scala.|[_root_.scala.Predef.String, _root_.scala.scalajs.js.typedarray.Uint8Array],
        _root_.scala.Null
      ],
      public_nonce: _root_.scala.scalajs.js.typedarray.Uint8Array,
      key: _root_.scala.scalajs.js.typedarray.Uint8Array,
      outputFormat: _root_.scala.scalajs.js.UndefOr[_root_.scala.|[
        libsodiumWrappersStrings.uint8array,
        _root_.scala.Null
      ]]
  ): _root_.scala.scalajs.js.typedarray.Uint8Array = js.native

  def fromBase64(input: _root_.scala.Predef.String): _root_.scala.scalajs.js.typedarray.Uint8Array = js.native

  def from_base64(
      input: _root_.scala.Predef.String,
      variant: base64Variants
  ): _root_.scala.scalajs.js.typedarray.Uint8Array = js.native

  def randombytes_buf(length: _root_.scala.Double): _root_.scala.scalajs.js.typedarray.Uint8Array = js.native

  def randombytes_buf(
      length: _root_.scala.Double,
      outputFormat: StringOutputFormat
  ): _root_.scala.Predef.String = js.native

  def randombytes_buf(
      length: _root_.scala.Double,
      outputFormat: libsodiumWrappersStrings.uint8array
  ): _root_.scala.scalajs.js.typedarray.Uint8Array = js.native

  def ready: _root_.scala.scalajs.js.Promise[_root_.scala.Unit] = js.native

  def to_base64(
      input: _root_.scala.scalajs.js.typedarray.Uint8Array,
      variant: base64Variants
  ): _root_.scala.Predef.String = js.native

  def toString(bytes: _root_.scala.scalajs.js.typedarray.Uint8Array): _root_.scala.Predef.String = js.native

  def crypto_aead_xchacha20poly1305_ietf_keygen(): _root_.scala.scalajs.js.typedarray.Uint8Array = js.native

  def crypto_aead_xchacha20poly1305_ietf_NPUBBYTES: _root_.scala.Double = js.native

  def crypto_aead_xchacha20poly1305_ietf_KEYBYTES: Double = js.native

}
