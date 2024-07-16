package lofi_acl.crypto

import lofi_acl.crypto.KeyDerivationKeyTest.{keysAreEqualAccordingToSignatureSubkey, testData}
import munit.FunSuite

import java.nio.charset.StandardCharsets

class KeyDerivationKeyTest extends FunSuite {
  private val kdk = KeyDerivationKey()

  test("key is not constant") {
    assert(!keysAreEqualAccordingToSignatureSubkey(kdk, KeyDerivationKey()))
  }

  test("kdk derivation") {
    assert(!keysAreEqualAccordingToSignatureSubkey(kdk.childKeyDerivationKey("Test"), kdk))
    assert(!keysAreEqualAccordingToSignatureSubkey(
      kdk.childKeyDerivationKey("A").childKeyDerivationKey("A"),
      kdk.childKeyDerivationKey("A")
    ))
  }

  test("recursive child key derivation") {
    assert(!keysAreEqualAccordingToSignatureSubkey(
      kdk.recursiveChildKeyDerivationKey(Array("A", "B", "C")),
      kdk.childKeyDerivationKey("A").childKeyDerivationKey("B").childKeyDerivationKey("C")
    ))
  }

  test("Encryption key derivation") {
    val encryptionKey = kdk.encryptionKey
    assertEquals(
      encryptionKey.decrypt(encryptionKey.encrypt("AE".getBytes(), "AD".getBytes()), "AD".getBytes()).toSeq,
      "AE".getBytes().toSeq
    )
  }

  test("Signing key derivation") {
    val signingKey = kdk.signingKey
    val sig        = Ed25519Util.sign(testData, signingKey.getPrivate)
    assert(Ed25519Util.checkEd25519Signature(testData, sig, kdk.signingKey.getPublic))
  }
}

object KeyDerivationKeyTest {
  private val testData = "TEST_TEST_TEST_TEST_TEST".getBytes(StandardCharsets.UTF_8)

  def keysAreEqualAccordingToSignatureSubkey(kdk1: KeyDerivationKey, kdk2: KeyDerivationKey): Boolean = {
    Ed25519Util.checkEd25519Signature(
      testData,
      Ed25519Util.sign(testData, kdk1.signingKey.getPrivate),
      kdk2.signingKey.getPublic
    )
  }
}
