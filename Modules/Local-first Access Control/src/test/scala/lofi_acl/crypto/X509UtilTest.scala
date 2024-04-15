package lofi_acl.crypto

import lofi_acl.crypto.X509Util.toPem
import munit.FunSuite
import org.bouncycastle.asn1.x500.style.BCStyle
import org.bouncycastle.cert.X509CertificateHolder
import org.bouncycastle.operator.bc.BcEdDSAContentVerifierProviderBuilder
import org.junit.Assert
import org.junit.Assert.assertArrayEquals

import java.io.ByteArrayInputStream
import java.security.cert.{CertificateFactory, X509Certificate}
import java.util.Base64

class X509UtilTest extends FunSuite {
  test("generateCustomP2PX509Certificate should have correct content") {
    val identityKey                       = Ed25519Util.generateNewKeyPair
    val certKey                           = Ed25519Util.generateNewKeyPair
    val certHolder: X509CertificateHolder = X509Util.generateCustomP2PX509Certificate(identityKey, certKey)

    // Is signed by identity
    val identityPublicKeyParam = Ed25519Util.keyPairToKeyParameters(identityKey)._1
    val certVerifier           = new BcEdDSAContentVerifierProviderBuilder().build(identityPublicKeyParam)
    assert(certHolder.isSignatureValid(certVerifier))

    val rdns = certHolder.getSubject.getRDNs(BCStyle.UNIQUE_IDENTIFIER)
    assertEquals(rdns.length, 1)
    assertEquals(rdns(0).getTypesAndValues.length, 1)
    val base64EncodedUnescapedIdentityPubKey = rdns(0).getFirst.getValue.toString
    val pubKeyBytes                          = Base64.getDecoder.decode(base64EncodedUnescapedIdentityPubKey)
    assertArrayEquals(pubKeyBytes, identityPublicKeyParam.getEncoded)
  }

  test("certificateToPublicIdentity should return identity") {
    val identityKey = Ed25519Util.generateNewKeyPair
    val certKey     = Ed25519Util.generateNewKeyPair
    val certificate = CertificateFactory
      .getInstance("X509")
      .generateCertificate(
        ByteArrayInputStream(
          X509Util.generateCustomP2PX509Certificate(identityKey, certKey).toPem.pemString.getBytes
        )
      )
      .asInstanceOf[X509Certificate]
    assertEquals(
      X509Util.certificateToPublicIdentity(certificate).id,
      Ed25519Util.publicKeyToPublicKeyBytesBase64Encoded(identityKey.getPublic)
    )
  }

  test("toPem should return parseable certificate in PEM format") {
    val identityKey                       = Ed25519Util.generateNewKeyPair
    val certKey                           = Ed25519Util.generateNewKeyPair
    val certHolder: X509CertificateHolder = X509Util.generateCustomP2PX509Certificate(identityKey, certKey)
    val pemString                         = certHolder.toPem.pemString

    List("BC", "SUN").foreach { providerName =>
      val cf         = CertificateFactory.getInstance("X.509", providerName)
      val parsedCert = cf.generateCertificate(ByteArrayInputStream(pemString.getBytes()))
      assert(parsedCert.isInstanceOf[X509Certificate])
      assertArrayEquals(parsedCert.getEncoded, certHolder.getEncoded)
    }
  }
}
