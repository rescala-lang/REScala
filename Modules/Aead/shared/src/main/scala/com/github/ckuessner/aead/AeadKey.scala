package com.github.ckuessner.aead

trait AeadKey {

  /** Instantiates an Aead instance that can encrypt and decrypt with this key.
    *
    * @return
    *   The Aead instance
    */
  def aeadPrimitive: Aead

  /** Returns the platform-native byte array representation of the raw key
    *
    * In the case of the JVM, this is an `Array[Byte]`, in the case of JS, this is a `Uint8Array`.
    *
    * @return
    *   The raw key byte array.
    */
  def rawKeyBytes: ByteArray

  /** Returns the raw key, encoded as a base64 string.
    *
    * @return
    *   A base64 string that represents the raw key.
    */
  def rawKeyBase64: String
}

object AeadKey {

  /** Instantiates an AeadKey instance for the provided raw key material
    *
    * @param rawKey
    *   the key as a byte array
    * @return
    *   the AeadKey instance
    */
  def fromRawKey(rawKey: ByteArray): AeadKey = AeadHelper.aeadKeyFromBytes(rawKey)

  /** Instantiates an AeadKey instance for the provided raw key material.
    *
    * The key is encoded as base64.
    *
    * @param base64EncodedKey
    *   the raw key encoded in base64
    * @return
    *   the AeadKey instance
    */
  def fromRawKey(base64EncodedKey: String): AeadKey = AeadHelper.aeadKeyFromBase64(base64EncodedKey)

  /** Generates a new AEAD key.
    *
    * @return
    *   the generated key
    */
  def generateKey: AeadKey = AeadHelper.generateKey
}
