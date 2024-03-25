package com.github.ckuessner.lofi_acl.crypto

import org.bouncycastle.asn1.DERUTF8String
import org.bouncycastle.asn1.edec.EdECObjectIdentifiers
import org.bouncycastle.asn1.x500.X500NameBuilder
import org.bouncycastle.asn1.x500.style.BCStyle
import org.bouncycastle.asn1.x509.{AlgorithmIdentifier, SubjectPublicKeyInfo}
import org.bouncycastle.cert.jcajce.{JcaX500NameUtil, JcaX509CertificateConverter}
import org.bouncycastle.cert.{X509CertificateHolder, X509v3CertificateBuilder}
import org.bouncycastle.crypto.params.{Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openssl.jcajce.JcaPEMWriter
import org.bouncycastle.operator.bc.BcEdECContentSignerBuilder

import java.io.StringWriter
import java.math.BigInteger
import java.security.cert.{CertificateException, X509Certificate}
import java.security.{KeyPair, Security}
import java.time.{ZoneId, ZonedDateTime}
import java.util.{Base64, Date}

object X509Util {
  if (Security.getProvider("BC") == null) {
    Security.addProvider(new BouncyCastleProvider())
  }

  private val algoIdEd25519 = new AlgorithmIdentifier(EdECObjectIdentifiers.id_Ed25519)

  def generateCustomP2PX509Certificate(privateIdentity: PrivateIdentity): X509CertificateHolder = {
    generateCustomP2PX509Certificate(privateIdentity.identityKey, privateIdentity.tlsKey)
  }

  /** Generates a new X.509 Certificate using the certificateKeyPair that is signed using the identityKeyPair. For the
    * subject of the generated certificate, the public key of the identityKeyPair is used as the UNIQUE_IDENTIFIER RDN.
    * [[https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.8 See RFC5280]]
    *
    * Note that the identityKeyPair must be an ed25519 key.
    *
    * @param certificateKeyPair
    *   The key that is used to create the certificate.
    * @param identityKeyPair
    *   The ed25519 key that is used to sign the certificate
    * @return
    *   The certificate alongside the newly generated Ed25519 keypair for the certificate.
    */
  def generateCustomP2PX509Certificate(identityKeyPair: KeyPair, certificateKeyPair: KeyPair): X509CertificateHolder = {

    if (!List("Ed25519", "EdDSA").contains(identityKeyPair.getPublic.getAlgorithm)) {
      throw IllegalArgumentException("Only Ed25519 keys are accepted as the identityKeyPair")
    }

    val identityBytes = Ed25519Util.publicKeyToRawPublicKeyBytes(identityKeyPair.getPublic)
    // Check that the identityKeyPair is in fact an Ed25519 key and not an Ed448 Key (SunEC used EdDSA algorithm identifier fo both Ed25519 and Ed448)
    if (identityBytes.length != 32) {
      throw IllegalArgumentException("Only Ed25519 keys are accepted as the identityKeyPair")
    }
    val identityBase64 = Base64.getEncoder.encodeToString(identityBytes)

    // We can use UNIQUE_IDENTIFIER to encode an arbitrary UTF-8 string without escaping (in contrast to CN)
    val identityName = X500NameBuilder().addRDN(BCStyle.UNIQUE_IDENTIFIER, identityBase64).build()
    val currentTime  = ZonedDateTime.now(ZoneId.of("GMT"))
    val validFrom    = currentTime.minusHours(1)
    val validUntil   = currentTime.plusYears(1)

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

  /** Extracts the public identity (Base64 of Ed25519 Public Key) from the certificate. Also verifies that it is
    * actually signed by public key of identity.
    *
    * @param certificate
    *   The certificate to extract the subject identity from.
    * @throws CertificateException
    *   if the validation failed
    */
  @throws[CertificateException]
  def certificateToPublicIdentity(certificate: X509Certificate): PublicIdentity = {
    val subject = certificate.getSubjectX500Principal
    val issuer  = certificate.getIssuerX500Principal

    if (!subject.equals(issuer)) throw new CertificateException("certificate not self issued")

    val uniqueIdRDNs = JcaX500NameUtil.getSubject(certificate).getRDNs(BCStyle.UNIQUE_IDENTIFIER)
    if (
      uniqueIdRDNs.isEmpty || uniqueIdRDNs(0).size() != 1 || !uniqueIdRDNs(0).getFirst.getValue
        .isInstanceOf[DERUTF8String]
    ) {
      throw new CertificateException("Subject does not contain a UNIQUE_IDENTIFIER")
    }

    // The base64 encoded public key
    val subjectUniqueIdentifier = uniqueIdRDNs(0).getFirst.getValue.asInstanceOf[DERUTF8String].toString
    val pubKeyBytes             = Base64.getDecoder.decode(subjectUniqueIdentifier)
    val pubKey                  = Ed25519Util.rawPublicKeyBytesToPublicKey(pubKeyBytes)

    // Verify cert instead of blindly returning identity
    try {
      certificate.verify(pubKey)
    } catch {
      case e: Exception => throw CertificateException(e)
    }

    PublicIdentity(subjectUniqueIdentifier)
  }

  extension (certHolder: X509CertificateHolder)
    def toPem: CertificatePem = {
      val writer    = new StringWriter()
      val pemWriter = new JcaPEMWriter(writer)
      val cert      = new JcaX509CertificateConverter().getCertificate(certHolder)
      pemWriter.writeObject(cert)
      pemWriter.close()
      CertificatePem(writer.toString)
    }

    def toJavaCertificate: X509Certificate = {
      new JcaX509CertificateConverter().getCertificate(certHolder)
    }
}
