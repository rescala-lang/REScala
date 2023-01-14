# Simple AEAD Library for Scala and Scala.js

This is a simple Scala library that provides authenticated encryption with associated data using the same interfaces on
the JVM and for Scala.js. The format of the keys and the encrypted messages is compatible between both platforms.

On the JVM, it relies on [Googles Tink Crypto Library](https://developers.google.com/tink) and for Scala.js, it
uses [libsodium.js](https://github.com/jedisct1/libsodium.js/), which brings [libsodium](https://doc.libsodium.org/) to
JavaScript.

## Crypto details

This library provides authenticated encryption with associated data using
the [XChaCha20-Poly1305](https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-xchacha)
cryptosystem. Nonces are randomly generated. The nonces have a length of 192 bits each, so reusing the same long-lived
key for encryption is secure for a practically unlimited number of messages and without any practical limit on the size
of the messages (up to `2^64` bytes). See the
[libsodium documentation](https://libsodium.gitbook.io/doc/secret-key_cryptography/aead/chacha20-poly1305/xchacha20-poly1305_construction)
for more details.

## Usage example

Note: On the JavaScript platform, the native type for a byte array is `Uint8Array`. On the JVM, the native type is
`Array[Byte]`. Because of this difference, this library defines a type alias `ByteArray`:

```scala
// JS Platform
type ByteArray = Uint8Array
// JVM Platform
type ByteArray = Array[Byte]
```

Here is an example on how this library can be used. Besides the import of the JSExecutionContext, this code works both
on the JVM, and on the JS platform.

```Scala
import com.github.ckuessner.aead.{Aead, AeadHelper, AeadKey, ByteArray}

import scala.concurrent.ExecutionContext
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue // Needed for AeadHelper.ready().andThen(…)
import scala.util.Try

object AeadExample extends App {

  // On the JS platform, you need check that libsodium.js is loaded before using any other functionality
  // On the JVM, calling ready() isn't necessary
  AeadHelper
    .ready()
    .andThen { _ =>
      // Generate a new key
      val key: AeadKey = AeadKey.generateKey

      // If you want to access the raw key
      val rawKey: ByteArray = key.rawKeyBytes
      // Alternatively encoded as Base64
      val rawKeyBase64: String = key.rawKeyBase64
      // To import it:
      AeadKey.fromRawKey(rawKey)
      // Or
      AeadKey.fromRawKey(rawKeyBase64)

      // If you want to encrypt / decrypt a message, you first need an Aead instance
      val aead: Aead = key.aeadPrimitive
      // Then you can encrypt and authenticate a message with (unencrypted but authenticated) associated data:
      val message = "This is a secret" // could also be a ByteArray
      val associatedData = "This is not secret" // could also be a ByteArray
      val ciphertext: ByteArray = aead.encrypt(message, associatedData).get // encrypt returns a Try[ByteArray]

      // If you want to encode it as base64
      val encodedCiphertext = AeadHelper.toBase64(ciphertext)
      // If you want to decode it from base64
      val decodedCiphertext = AeadHelper.fromBase64(encodedCiphertext)

      // To decrypt it:
      val decryptedMessage: Try[String] = aead.decrypt(ciphertext, associatedData)
    }
}
```
