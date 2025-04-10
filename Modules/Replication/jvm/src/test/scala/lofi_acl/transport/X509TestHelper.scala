package lofi_acl.transport

import crypto.Ed25519Util
import org.bouncycastle.asn1.edec.EdECObjectIdentifiers
import org.bouncycastle.asn1.x500.X500NameBuilder
import org.bouncycastle.asn1.x500.style.BCStyle
import org.bouncycastle.asn1.x509.{AlgorithmIdentifier, SubjectPublicKeyInfo}
import org.bouncycastle.cert.{X509CertificateHolder, X509v3CertificateBuilder}
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters
import org.bouncycastle.operator.bc.BcEdECContentSignerBuilder

import java.math.BigInteger
import java.security.KeyPair
import java.time.{ZoneId, ZonedDateTime}
import java.util.{Base64, Date}

object X509TestHelper {
  private val algoIdEd25519 = new AlgorithmIdentifier(EdECObjectIdentifiers.id_Ed25519)

  def genCertSignedByWrongKey(identityKeyPair: KeyPair, certificateKeyPair: KeyPair): X509CertificateHolder = {
    val identityBytes  = Ed25519Util.publicKeyToRawPublicKeyBytes(identityKeyPair.getPublic)
    val identityBase64 = Base64.getEncoder.encodeToString(identityBytes)
    val identityName   = X500NameBuilder().addRDN(BCStyle.UNIQUE_IDENTIFIER, identityBase64).build()
    val currentTime    = ZonedDateTime.now(ZoneId.of("GMT"))
    val validFrom      = currentTime.minusHours(1)
    val validUntil     = currentTime.plusYears(1)

    // Different key from identity
    val signingKey                                         = Ed25519Util.generateNewKeyPair
    val signerPrivateKeyParam: Ed25519PrivateKeyParameters = Ed25519Util.keyPairToKeyParameters(signingKey)._2
    val signer = new BcEdECContentSignerBuilder(algoIdEd25519).build(signerPrivateKeyParam)

    new X509v3CertificateBuilder(
      identityName,
      BigInteger.valueOf(System.currentTimeMillis()),
      Date.from(validFrom.toInstant),
      Date.from(validUntil.toInstant),
      identityName,
      SubjectPublicKeyInfo(algoIdEd25519, Ed25519Util.publicKeyToRawPublicKeyBytes(certificateKeyPair.getPublic))
    ).build(signer)
  }

  def genCertWithoutUniqueId(identityKeyPair: KeyPair, certificateKeyPair: KeyPair): X509CertificateHolder = {
    val identityBytes  = Ed25519Util.publicKeyToRawPublicKeyBytes(identityKeyPair.getPublic)
    val identityBase64 = Base64.getEncoder.encodeToString(identityBytes)
    val identityName   = X500NameBuilder().addRDN(BCStyle.CN /* <- CN, NOT UNIQUE_ID */, identityBase64).build()
    val currentTime    = ZonedDateTime.now(ZoneId.of("GMT"))
    val validFrom      = currentTime.minusHours(1)
    val validUntil     = currentTime.plusYears(1)

    val (_, signerPrivateKeyParam) = Ed25519Util.keyPairToKeyParameters(identityKeyPair)
    val signer                     = new BcEdECContentSignerBuilder(algoIdEd25519).build(signerPrivateKeyParam)

    new X509v3CertificateBuilder(
      identityName,
      BigInteger.valueOf(System.currentTimeMillis()),
      Date.from(validFrom.toInstant),
      Date.from(validUntil.toInstant),
      identityName,
      SubjectPublicKeyInfo(algoIdEd25519, Ed25519Util.publicKeyToRawPublicKeyBytes(certificateKeyPair.getPublic))
    ).build(signer)
  }

  def genExpiredCert(identityKeyPair: KeyPair, certificateKeyPair: KeyPair): X509CertificateHolder = {
    val identityBytes  = Ed25519Util.publicKeyToRawPublicKeyBytes(identityKeyPair.getPublic)
    val identityBase64 = Base64.getEncoder.encodeToString(identityBytes)
    val identityName   = X500NameBuilder().addRDN(BCStyle.CN, identityBase64).build()
    val currentTime    = ZonedDateTime.now(ZoneId.of("GMT"))
    val validFrom      = currentTime.minusHours(2)
    val validUntil     = currentTime.minusHours(1) // validity ends in the past

    val (_, signerPrivateKeyParam) = Ed25519Util.keyPairToKeyParameters(identityKeyPair)
    val signer                     = new BcEdECContentSignerBuilder(algoIdEd25519).build(signerPrivateKeyParam)

    new X509v3CertificateBuilder(
      identityName,
      BigInteger.valueOf(System.currentTimeMillis()),
      Date.from(validFrom.toInstant),
      Date.from(validUntil.toInstant),
      identityName,
      SubjectPublicKeyInfo(algoIdEd25519, Ed25519Util.publicKeyToRawPublicKeyBytes(certificateKeyPair.getPublic))
    ).build(signer)
  }
}
