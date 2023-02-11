package com.github.ckuessner.aead

import com.github.ckuessner.aead.ByteArray

import scala.util.Try

/** The Aead type provides a simple interface for authenticated encryption with associated data.
  *
  * The underlying crypto system is XChaCha20-Poly1305, as described in
  * [[https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-xchacha XChaCha: eXtended-nonce ChaCha and AEAD_XChaCha20_Poly1305]]
  *
  * Encryption relies on random nonces. Nonces have a length of 192 bits each, so reusing the same long-lived key for
  * encryption is secure for a practically unlimited number of messages and without any practical limit on the size of
  * the messages (up to `2^64` bytes). See the
  * [[https://libsodium.gitbook.io/doc/secret-key_cryptography/aead/chacha20-poly1305/xchacha20-poly1305_construction libsodium documentation]]
  * for more dertails.
  */
trait Aead {

  /** Encrypts and authenticates a message along the authenticated non-confidential associated data.
    *
    * NOTE: The associated data is not encrypted, only authenticated! Only the message is encrypted.
    *
    * @param plaintext
    *   plain text message that will be encrypted and authenticated
    * @param associatedData
    *   non-confidential associated data to be authenticated along the message
    * @return
    *   the encrypted message, or a Failure
    */
  def encrypt(plaintext: ByteArray, associatedData: ByteArray): Try[ByteArray]

  /** Encrypts and authenticates a message along the authenticated non-confidential associated data.
    *
    * NOTE: The associated data is not encrypted, only authenticated! Only the message is encrypted.
    *
    * @param plaintext
    *   plain text message that will be encrypted and authenticated
    * @param associatedData
    *   non-confidential associated data to be authenticated along the message
    * @return
    *   the encrypted message, or a Failure
    */
  def encrypt(plaintext: String, associatedData: String): Try[ByteArray]

  /** Decrypts the encrypted message using the provided key, checking the authenticity of the ciphertext and associated
    * data.
    *
    * @param ciphertext
    *   The ciphertext and used nonce of the encrypted message
    * @param associatedData
    *   The plaintext of the associated data that is authenticated along the encrypted message
    * @return
    *   The plaintext of the encrypted message if the encryption was successful, or a Failure
    */
  def decrypt(ciphertext: ByteArray, associatedData: ByteArray): Try[ByteArray]

  /** Decrypts the encrypted message using the provided key, checking the authenticity of the ciphertext and associated
    * data.
    *
    * The plain text message is assumed to be a UTF-8 String.
    *
    * @param ciphertext
    *   The ciphertext and used nonce of the encrypted message
    * @param associatedData
    *   The plaintext of the associated data that is authenticated along the encrypted message
    * @return
    *   The plaintext of the encrypted message if the encryption was successful, or a Failure
    */
  def decrypt(ciphertext: ByteArray, associatedData: String): Try[String]
}
