package crypto

import com.google.crypto.tink.subtle.{ChaCha20Poly1305, X25519}
import munit.FunSuite
import org.bouncycastle.crypto.generators.HKDFBytesGenerator
import org.bouncycastle.crypto.params.HKDFParameters
import org.bouncycastle.crypto.util.DigestFactory

class X25519Test extends FunSuite {
  test("Key exchange with derivation works") {
    val alicePrivateKey = X25519.generatePrivateKey()
    val alicePublicKey  = X25519.publicFromPrivate(alicePrivateKey)

    val bobPrivateKey = X25519.generatePrivateKey()
    val bobPublicKey  = X25519.publicFromPrivate(bobPrivateKey)

    val aliceSharedSecret = X25519.computeSharedSecret(alicePrivateKey, bobPublicKey)
    val bobSharedSecret   = X25519.computeSharedSecret(bobPrivateKey, alicePublicKey)

    assertEquals(aliceSharedSecret.toSeq, bobSharedSecret.toSeq)

    // Don't skip extraction step!
    val encryptionKey = {
      val hkdfParameters = HKDFParameters(bobSharedSecret, null, "CAPABILITY".getBytes())
      val hkdf           = HKDFBytesGenerator(DigestFactory.createSHA256())
      hkdf.init(hkdfParameters)
      val rawKey = Array.ofDim[Byte](32)
      hkdf.generateBytes(rawKey, 0, 32)
      ChaCha20Poly1305(rawKey)
    }

    val ciphertext = encryptionKey.encrypt("Hello World".getBytes, Array.empty)

    val shouldBeTheSameEncryptionKey = {
      val hkdfParameters = HKDFParameters(aliceSharedSecret, null, "CAPABILITY".getBytes())
      val hkdf           = HKDFBytesGenerator(DigestFactory.createSHA256())
      hkdf.init(hkdfParameters)
      val rawKey = Array.ofDim[Byte](32)
      hkdf.generateBytes(rawKey, 0, 32)
      ChaCha20Poly1305(rawKey)
    }

    assertEquals(shouldBeTheSameEncryptionKey.decrypt(ciphertext, Array.empty).toSeq, "Hello World".getBytes().toSeq)
  }
}
