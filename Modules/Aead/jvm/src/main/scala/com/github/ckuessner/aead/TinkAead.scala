package com.github.ckuessner.aead

import java.nio.charset.StandardCharsets
import scala.util.Try

class TinkAead(private val googleAead: com.google.crypto.tink.Aead) extends Aead {
  override def encrypt(plaintext: Array[Byte], associatedData: Array[Byte]): Try[Array[Byte]] = Try {
    googleAead.encrypt(plaintext, associatedData)
  }

  override def encrypt(plaintext: String, associatedData: String): Try[Array[Byte]] = Try {
    val plainTextBytes      = plaintext.getBytes(StandardCharsets.UTF_8)
    val associatedDataBytes = associatedData.getBytes(StandardCharsets.UTF_8)
    googleAead.encrypt(plainTextBytes, associatedDataBytes)
  }

  override def decrypt(ciphertext: Array[Byte], associatedData: Array[Byte]): Try[Array[Byte]] = Try {
    googleAead.decrypt(ciphertext, associatedData)
  }

  override def decrypt(ciphertext: Array[Byte], associatedData: String): Try[String] = {
    decrypt(ciphertext, associatedData.getBytes(StandardCharsets.UTF_8)).map { plaintextBytes =>
      new String(plaintextBytes, StandardCharsets.UTF_8)
    }
  }
}
