package lofi_acl.crypto

import com.google.crypto.tink.subtle.XChaCha20Poly1305
import munit.FunSuite
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.generators.HKDFBytesGenerator
import org.bouncycastle.crypto.params.HKDFParameters

import java.nio.charset.StandardCharsets
import java.security.SecureRandom

class HkdfTests extends FunSuite {
  test("BouncyCastle hkdf for AEAD with XChaCha20-Poly1305") {
    def deriveKey(inputKeyMaterial: Array[Byte], info: Array[Byte]): Array[Byte] = {
      // We don't need to extract, since we have uniformly distributed input-key-material
      val hkdfParameters = HKDFParameters.skipExtractParameters(inputKeyMaterial, info)
      val hkdf           = HKDFBytesGenerator(SHA256Digest())
      hkdf.init(hkdfParameters)
      val outputKeyMaterial: Array[Byte] = Array.ofDim(32)
      hkdf.generateBytes(outputKeyMaterial, 0, 32)
      outputKeyMaterial
    }

    def encrypt(
        message: Array[Byte],
        associatedData: Array[Byte],
        inputKeyMaterial: Array[Byte],
        info: Array[Byte]
    ): Array[Byte] = {
      val encryptionKey = deriveKey(inputKeyMaterial, info)
      val derivedKey    = XChaCha20Poly1305(encryptionKey)
      derivedKey.encrypt(message, associatedData)
    }

    def decrypt(
        cipherText: Array[Byte],
        associatedData: Array[Byte],
        ikm: Array[Byte],
        info: Array[Byte]
    ): Array[Byte] = {
      val decryptionKey = deriveKey(ikm, info)
      val derivedKey    = XChaCha20Poly1305(decryptionKey)
      derivedKey.decrypt(cipherText, associatedData)
    }

    val inputKeyMaterial: Array[Byte] = Array.ofDim(32) // 256 bits
    SecureRandom().nextBytes(inputKeyMaterial)
    val info = "Encryption Key".getBytes(StandardCharsets.UTF_8)

    val msg            = "Secret Message".getBytes(StandardCharsets.UTF_8)
    val associatedData = "Public Metadata".getBytes(StandardCharsets.UTF_8)

    val ciphertext = encrypt(msg, associatedData, inputKeyMaterial, info)
    assertEquals(
      msg.toSeq,
      decrypt(ciphertext, associatedData, inputKeyMaterial, info).toSeq
    )
  }

  test("Combining keys") {
    val inputKeyMaterial: Array[Byte] = Array.ofDim(32) // 256 bits
    val kdkPart1 = {
      val hkdfParameters = HKDFParameters.skipExtractParameters(inputKeyMaterial, "PART A".getBytes())
      val hkdf           = HKDFBytesGenerator(SHA256Digest())
      hkdf.init(hkdfParameters)
      val outputKeyMaterial: Array[Byte] = Array.ofDim(32)
      hkdf.generateBytes(outputKeyMaterial, 0, 32)
      outputKeyMaterial
    }
    val kdkPart2 = {
      val hkdfParameters = HKDFParameters.skipExtractParameters(inputKeyMaterial, "PART B".getBytes())
      val hkdf           = HKDFBytesGenerator(SHA256Digest())
      hkdf.init(hkdfParameters)
      val outputKeyMaterial: Array[Byte] = Array.ofDim(32)
      hkdf.generateBytes(outputKeyMaterial, 0, 32)
      outputKeyMaterial
    }
    val okm = {
      val combined       = kdkPart1.concat(kdkPart2)
      val hkdfParameters = HKDFParameters.skipExtractParameters(combined, "COMBINATION".getBytes())
      val hkdf           = HKDFBytesGenerator(SHA256Digest())
      hkdf.init(hkdfParameters)
      val outputKeyMaterial: Array[Byte] = Array.ofDim(32)
      hkdf.generateBytes(outputKeyMaterial, 0, 32)
      outputKeyMaterial
    }
  }

}
