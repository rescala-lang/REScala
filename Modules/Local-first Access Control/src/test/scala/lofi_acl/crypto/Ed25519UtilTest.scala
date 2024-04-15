package lofi_acl.crypto

import munit.FunSuite
import org.bouncycastle.crypto.params.{Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import org.bouncycastle.crypto.signers.Ed25519Signer
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.junit.Assert
import org.junit.Assert.assertArrayEquals

import java.security.*
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.util.Base64

class Ed25519UtilTest extends FunSuite {
  if (Security.getProvider("BC") == null) {
    Security.addProvider(new BouncyCastleProvider())
  }

  private val cryptoProviders = List("BC", "SunEC")

  test("generateNewKeyPair") {
    Ed25519Util.generateNewKeyPair
  }

  test("generateNewKeyParameters") {
    Ed25519Util.generateNewKeyParameters
  }

  test("keyPairToKeyParameters should work with all providers") {
    cryptoProviders.foreach { providerName =>
      val keyPair                     = KeyPairGenerator.getInstance("Ed25519", providerName).generateKeyPair()
      val (pubKeyParam, privKeyParam) = Ed25519Util.keyPairToKeyParameters(keyPair)

      val signer = new Ed25519Signer()
      signer.init(true, Ed25519PrivateKeyParameters(privKeyParam.getEncoded))
      signer.update("Test".getBytes, 0, "Test".length)
      val sig = signer.generateSignature()

      val jcaVerifier = Signature.getInstance("Ed25519")
      jcaVerifier.initVerify(keyPair.getPublic)
      jcaVerifier.update("Test".getBytes)
      assert(jcaVerifier.verify(sig))

      val verifier = new Ed25519Signer()
      verifier.init(false, Ed25519PublicKeyParameters(pubKeyParam.getEncoded))
      verifier.update("Test".getBytes, 0, "Test".length)
      assert(verifier.verifySignature(sig))
    }
  }

  test("publicKeyToRawPublicKeyBytes should work with BC and SunEC providers") {
    cryptoProviders.foreach { keyGenProvider =>
      val keyPair = KeyPairGenerator.getInstance("Ed25519", keyGenProvider).generateKeyPair()
      val rawKey  = Ed25519Util.publicKeyToRawPublicKeyBytes(keyPair.getPublic)
      assertEquals(rawKey.length, 32)

      cryptoProviders.foreach { verifyProvider =>
        val parsedPubKey = Ed25519Util.rawPublicKeyBytesToPublicKey(rawKey)
        testKeyEqualityUsingSignature(parsedPubKey, keyPair.getPrivate, keyGenProvider, verifyProvider)
      }
    }
  }

  test("privateKeyToPkcs8EncodedPrivateKeyBytes should work with BC and SunEC providers") {
    cryptoProviders.foreach { providerName =>
      val gen               = KeyPairGenerator.getInstance("Ed25519", providerName)
      val keyPair           = gen.generateKeyPair()
      val encodedPrivateKey = Ed25519Util.privateKeyToPkcs8EncodedPrivateKeyBytes(keyPair.getPrivate)

      cryptoProviders.foreach { otherProvider =>
        val spec                  = new PKCS8EncodedKeySpec(encodedPrivateKey, "Ed25519")
        val parsedKey: PrivateKey = KeyFactory.getInstance("Ed25519", otherProvider).generatePrivate(spec)
        testKeyEqualityUsingSignature(
          keyPair.getPublic,
          parsedKey,
          signProvider = otherProvider,
          verifyProvider = providerName
        )
      }
    }
  }

  test("privateKeyToPemString should return PEM string that is parseable and results in same key") {
    cryptoProviders.foreach { providerName =>
      val keyPair = KeyPairGenerator.getInstance("Ed25519", providerName).generateKeyPair()
      val pem     = Ed25519Util.privateKeyToPem(keyPair.getPrivate).pemString
      assert(pem.startsWith("-----BEGIN PRIVATE KEY-----" + System.lineSeparator()))
      assert(pem.endsWith("-----END PRIVATE KEY-----" + System.lineSeparator()))
      val pkcs8Bytes = Base64.getMimeDecoder.decode(
        pem.substring(
          ("-----BEGIN PRIVATE KEY-----" + System.lineSeparator()).length,
          pem.length - ("-----END PRIVATE KEY-----" + System.lineSeparator()).length
        )
      )

      cryptoProviders.foreach { decodeProviderName =>
        val spec                  = new PKCS8EncodedKeySpec(pkcs8Bytes, "Ed25519")
        val parsedKey: PrivateKey = KeyFactory.getInstance("Ed25519", providerName).generatePrivate(spec)

        testKeyEqualityUsingSignature(
          keyPair.getPublic,
          parsedKey,
          signProvider = decodeProviderName,
          verifyProvider = providerName
        )
      }
    }
  }

  test("publicKeyToPemString return PEM string that is parseable and results in same public key") {
    cryptoProviders.foreach { providerName =>
      val keyPair = KeyPairGenerator.getInstance("Ed25519", providerName).generateKeyPair()
      val pem     = Ed25519Util.publicKeyToPemString(keyPair.getPublic)
      assert(pem.startsWith("-----BEGIN PUBLIC KEY-----" + System.lineSeparator()))
      assert(pem.endsWith("-----END PUBLIC KEY-----" + System.lineSeparator()))

      val x509EncodedBytes = Base64.getMimeDecoder.decode(
        pem.substring(
          ("-----BEGIN PUBLIC KEY-----" + System.lineSeparator()).length,
          pem.length - ("-----END PUBLIC KEY-----" + System.lineSeparator()).length
        )
      )

      cryptoProviders.foreach { decodeProviderName =>
        val spec      = new X509EncodedKeySpec(x509EncodedBytes, "Ed25519")
        val parsedKey = KeyFactory.getInstance("Ed25519", decodeProviderName).generatePublic(spec)
        assertArrayEquals(parsedKey.getEncoded, keyPair.getPublic.getEncoded)
      }
    }

  }

  test("testEqualityUsingSignature should work with same key") {
    cryptoProviders.flatMap(a => cryptoProviders.map(b => (a, b))).foreach { (providerA, providerB) =>
      val keyA = Ed25519Util.generateNewKeyPair
      testKeyEqualityUsingSignature(keyA.getPublic, keyA.getPrivate, providerA, providerB)
    }
  }

  test("testEqualityUsingSignature should fail with different key") {
    cryptoProviders.flatMap(a => cryptoProviders.map(b => (a, b))).foreach { (providerA, providerB) =>
      val keyA = Ed25519Util.generateNewKeyPair
      val keyB = Ed25519Util.generateNewKeyPair
      intercept[AssertionError] {
        testKeyEqualityUsingSignature(keyA.getPublic, keyB.getPrivate, providerA, providerB)
      }
    }
  }

  private def testKeyEqualityUsingSignatureWithAllProviders(keyPairA: KeyPair, keyPairB: KeyPair): Unit = {
    cryptoProviders.foreach { providerNameVerify =>
      cryptoProviders.foreach { providerNameSign =>
        testKeyEqualityUsingSignature(keyPairA.getPublic, keyPairB.getPrivate, providerNameSign, providerNameVerify)
        testKeyEqualityUsingSignature(keyPairB.getPublic, keyPairA.getPrivate, providerNameSign, providerNameVerify)
      }
    }
  }

  private def testKeyEqualityUsingSignature(
      pubKey: PublicKey,
      privateKey: PrivateKey,
      signProvider: String,
      verifyProvider: String
  ): Unit = {
    val signer = Signature.getInstance("Ed25519", signProvider)
    signer.initSign(privateKey)
    signer.update("Test".getBytes())
    val signature = signer.sign()

    val verifier = Signature.getInstance("Ed25519", verifyProvider)
    verifier.initVerify(pubKey)
    verifier.update("Test".getBytes())
    assert(verifier.verify(signature))
  }
}
