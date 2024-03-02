package com.github.ckuessner.aead

import scala.concurrent.Future
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.undefined
import scala.util.Try

/** Helper object to access the XChaCha20-Poly1305 AEAD primitives of the
  * [[https://github.com/jedisct1/libsodium.js libsodium.js]] library.
  *
  * Encryption relies on random nonces. Nonces have a length of 192 bits each, so reusing the same long-lived key for
  * encryption is secure for a practically unlimited number of messages and without any practical limit on the size of
  * the messages (up to `2^64` bytes). See the
  * [[https://libsodium.gitbook.io/doc/secret-key_cryptography/aead/chacha20-poly1305/xchacha20-poly1305_construction libsodium documentation]].
  */
object AeadHelper {

  /** Checks that libsodium.js is loaded. */
  def ready(): Future[Unit] = {
    sodium.ready.toFuture
  }

  def toBase64(bytes: Uint8Array): String = sodium.to_base64(bytes, base64Variants.ORIGINAL)

  def fromBase64(bytes: String): Uint8Array = sodium.from_base64(bytes, base64Variants.ORIGINAL)

  /** Generates a new AeadKey.
    *
    * @return
    *   An AeadKey instance with newly generated key material.
    */
  def generateKey: LibsodiumJsAeadKey = {
    LibsodiumJsAeadKey(generateRawKey)
  }

  /** Generates a new XChaCha20-Poly1305 key.
    *
    * @return
    *   The new key
    */
  def generateRawKey: Uint8Array = {
    sodium.crypto_aead_xchacha20poly1305_ietf_keygen()
  }

  /** Instantiates an AeadKey instance for the provided raw key material
    *
    * @param rawKey
    *   the key as a typed array
    * @return
    *   the AeadKey instance
    */
  def aeadKeyFromBytes(rawKey: Uint8Array): LibsodiumJsAeadKey = {
    LibsodiumJsAeadKey(rawKey)
  }

  /** Instantiates an AeadKey instance for the provided raw key material
    *
    * @param rawKey
    *   the key as a byte array
    * @return
    *   the AeadKey instance
    */
  def aeadKeyFromBase64(rawKey: String): LibsodiumJsAeadKey = {
    LibsodiumJsAeadKey(
      sodium.from_base64(rawKey, base64Variants.ORIGINAL)
    )
  }

  /** Generates a new random nonce.
    *
    * @return
    *   A random nonce
    */
  private inline def generateRandomNonce(): Uint8Array = {
    sodium.randombytes_buf(sodium.crypto_aead_xchacha20poly1305_ietf_NPUBBYTES)
  }

  /** Encrypts and authenticates a message along the authenticated non-confidential associated data.
    *
    * NOTE: The associated data is not encrypted, only authenticated! Only the message is encrypted.
    *
    * @param message
    *   plain text message that is encrypted and authenticated
    * @param associatedData
    *   non-confidential associated data to be authenticated along the message
    * @param key
    *   private key used for encryption and authentication
    * @return
    *   the encrypted message, or a Failure
    */
  def encrypt(message: String, associatedData: String, key: Uint8Array): Try[Uint8Array] = Try {
    val nonce: Uint8Array = generateRandomNonce()
    val cipherText =
      sodium.crypto_aead_xchacha20poly1305_ietf_encrypt(message, associatedData, null, nonce, key, undefined)
    concatenateArrays(nonce, cipherText)
  }

  /** Encrypts and authenticates a message along the authenticated non-confidential associated data using the key.
    *
    * NOTE: The associated data is not encrypted, only authenticated! Only the message is encrypted.
    *
    * @param message
    *   plain text message that is encrypted and authenticated
    * @param associatedData
    *   non-confidential associated data to be authenticated along the message
    * @param key
    *   private key used for encryption and authentication
    * @return
    *   the encrypted message, or a Failure
    */
  def encrypt(message: Uint8Array, associatedData: Uint8Array, key: Uint8Array): Try[Uint8Array] = Try {
    val nonce: Uint8Array = generateRandomNonce()
    val cipherText =
      sodium.crypto_aead_xchacha20poly1305_ietf_encrypt(message, associatedData, null, nonce, key, undefined)
    concatenateArrays(nonce, cipherText)
  }

  /** Decrypts the encrypted message using the provided key, checking the authenticity of the ciphertext and associated
    * data.
    *
    * @param encryptedMessage
    *   The ciphertext and nonce
    * @param associatedData
    *   The plaintext of the associated data that is authenticated along the encrypted message
    * @param key
    *   The private key used for authentication and encryption
    * @return
    *   The plaintext of the encrypted message if the encryption was successful, or a Failure
    */
  def decrypt(encryptedMessage: Uint8Array, associatedData: Uint8Array, key: Uint8Array): Try[Uint8Array] = {
    val (usedNonce, cipherText) = extractNonceAndCiphertext(encryptedMessage)
    Try {
      sodium.crypto_aead_xchacha20poly1305_ietf_decrypt(null, cipherText, associatedData, usedNonce, key, undefined)
    }
  }

  /** Decrypts the encrypted message using the provided key, checking the authenticity of the ciphertext and associated
    * data.
    *
    * @param encryptedMessage
    *   The ciphertext and nonce
    * @param associatedData
    *   The plaintext of the associated data that is authenticated along the encrypted message
    * @param key
    *   The private key used for authentication and encryption
    * @return
    *   The plaintext of the encrypted message if the encryption was successful, or a Failure
    */
  def decrypt(encryptedMessage: Uint8Array, associatedData: String, key: Uint8Array): Try[String] = {
    val (usedNonce: Uint8Array, cipherText: Uint8Array) = extractNonceAndCiphertext(encryptedMessage)
    Try {
      sodium.crypto_aead_xchacha20poly1305_ietf_decrypt(
        null,
        cipherText,
        associatedData,
        usedNonce,
        key,
        StringOutputFormat.text
      )
    }
  }

  private inline def concatenateArrays(nonce: Uint8Array, ciphertext: Uint8Array): Uint8Array = {
    val concatenatedArray = new Uint8Array(nonce.length + ciphertext.length)
    concatenatedArray.set(nonce)
    concatenatedArray.set(ciphertext, nonce.length)
    concatenatedArray
  }

  private inline def extractNonceAndCiphertext(combinedCipherText: Uint8Array): (Uint8Array, Uint8Array) = {
    val nonceLength: Int       = sodium.crypto_aead_xchacha20poly1305_ietf_NPUBBYTES.toInt
    val usedNonce: Uint8Array  = combinedCipherText.subarray(0, nonceLength)
    val cipherText: Uint8Array = combinedCipherText.subarray(nonceLength, combinedCipherText.length)
    (usedNonce, cipherText)
  }

}
