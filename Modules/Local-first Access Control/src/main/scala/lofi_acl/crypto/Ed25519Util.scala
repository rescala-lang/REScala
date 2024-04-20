package lofi_acl.crypto

import org.bouncycastle.asn1.edec.EdECObjectIdentifiers
import org.bouncycastle.asn1.pkcs.PrivateKeyInfo
import org.bouncycastle.asn1.x509.{AlgorithmIdentifier, SubjectPublicKeyInfo}
import org.bouncycastle.asn1.{ASN1InputStream, DEROctetString}
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.util.io.pem.{PemObject, PemWriter}

import java.io.{ByteArrayInputStream, StringWriter}
import java.security.*
import java.security.spec.X509EncodedKeySpec
import java.util.Base64

object Ed25519Util {
  if (Security.getProvider("BC") == null) {
    val _ = Security.addProvider(new BouncyCastleProvider())
  }

  private val ed25519AlgoIdentifier = new AlgorithmIdentifier(EdECObjectIdentifiers.id_Ed25519)

  // The keyParamGenerator is thread safe, since the nextBytes(…) method of the SecureRandom is thread safe.
  private val keyParamGenerator: Ed25519KeyPairGenerator = {
    val generator = new Ed25519KeyPairGenerator
    generator.init(new Ed25519KeyGenerationParameters(new SecureRandom()))
    generator
  }

  // The keyPairGenerator is thread safe, since the nextBytes(…) method of the SecureRandom is thread safe.
  private val keyPairGenerator: KeyPairGenerator = {
    KeyPairGenerator.getInstance("Ed25519", "BC")
  }

  /** Generates a new Ed25519 KeyPair.
    *
    * @return
    *   the newly generated KeyPair.
    */
  def generateNewKeyPair: KeyPair = {
    keyPairGenerator.generateKeyPair()
  }

  private[crypto] def generateNewKeyParameters: (Ed25519PublicKeyParameters, Ed25519PrivateKeyParameters) = {
    val key = keyParamGenerator.generateKeyPair()
    (key.getPublic.asInstanceOf[Ed25519PublicKeyParameters], key.getPrivate.asInstanceOf[Ed25519PrivateKeyParameters])
  }

  private[crypto] def keyPairToKeyParameters(
      keyPair: KeyPair
  ): (Ed25519PublicKeyParameters, Ed25519PrivateKeyParameters) = {
    if ("Ed25519" != keyPair.getPrivate.getAlgorithm && "EdDSA" != keyPair.getPrivate.getAlgorithm) {
      throw IllegalArgumentException(s"Not an Ed25519 key, but a ${keyPair.getPrivate.getAlgorithm}")
    }

    val pubKeyBytes     = publicKeyToRawPublicKeyBytes(keyPair.getPublic)
    val privKeyInfo     = PrivateKeyInfo.getInstance(keyPair.getPrivate.getEncoded)
    val privateKeyBytes = privKeyInfo.parsePrivateKey().asInstanceOf[DEROctetString].getOctets

    (new Ed25519PublicKeyParameters(pubKeyBytes), new Ed25519PrivateKeyParameters(privateKeyBytes))
  }

  def base64PublicKeyBytesToPublicKey(publicKeyBytesInBase64: String): PublicKey = {
    rawPublicKeyBytesToPublicKey(Base64.getDecoder.decode(publicKeyBytesInBase64))
  }

  def rawPublicKeyBytesToPublicKey(ed25519Bytes: Array[Byte]): PublicKey = {
    val encodedPubKey = new SubjectPublicKeyInfo(ed25519AlgoIdentifier, ed25519Bytes)
    val publicKeySpec = new X509EncodedKeySpec(encodedPubKey.getEncoded())
    KeyFactory.getInstance("Ed25519", "BC").generatePublic(publicKeySpec)
  }

  def publicKeyToPublicKeyBytesBase64Encoded(publicKey: PublicKey): String = {
    Base64.getEncoder.encodeToString(publicKeyToRawPublicKeyBytes(publicKey))
  }

  def publicKeyToRawPublicKeyBytes(publicKey: PublicKey): Array[Byte] = {
    if ("Ed25519" != publicKey.getAlgorithm && "EdDSA" != publicKey.getAlgorithm) {
      throw IllegalArgumentException(s"Expected an Ed25519 key, got ${publicKey.getAlgorithm}")
    }
    val asn1Primitive = new ASN1InputStream(new ByteArrayInputStream(publicKey.getEncoded)).readObject()
    val bytes         = SubjectPublicKeyInfo.getInstance(asn1Primitive).getPublicKeyData.getBytes
    if (bytes.length != 32)
      throw IllegalArgumentException(s"Ed25519 public keys are 32 bytes long, got ${bytes.length}")
    bytes
  }

  def privateKeyToPkcs8EncodedPrivateKeyBytes(ed25519PrivateKey: PrivateKey): Array[Byte] = {
    if (!List("Ed25519", "EdDSA").contains(ed25519PrivateKey.getAlgorithm)) {
      throw IllegalArgumentException(s"Expected an Ed25519 or EdDSA key, got ${ed25519PrivateKey.getAlgorithm}")
    }
    if ("PKCS#8" != ed25519PrivateKey.getFormat) {
      throw RuntimeException(s"privateKey does not support PKCS#8 encoding, but uses ${ed25519PrivateKey.getFormat}")
    }
    ed25519PrivateKey.getEncoded
  }

  /** Encodes PrivateKey as PEM string with PKCS#8 encoded private key.
    *
    * @param privateKey
    *   The private key to encode
    * @return
    *   the PKCS#8-PEM encoded private key
    */
  def privateKeyToPem(privateKey: PrivateKey): PrivateKeyPem = {
    if ("PKCS#8" != privateKey.getFormat) throw RuntimeException("Given private key doesn't use PKCS#8 encoding")
    val writer    = new StringWriter()
    val pemWriter = new PemWriter(writer)
    pemWriter.writeObject(new PemObject("PRIVATE KEY", privateKey.getEncoded))
    pemWriter.close()
    PrivateKeyPem(writer.toString)
  }

  /** Encodes the public key in X.509 PEM format.
    *
    * @param publicKey
    *   The public key to encode
    * @return
    *   the X.509-PEM-encoded public key as a string
    */
  def publicKeyToPemString(publicKey: PublicKey): String = {
    val writer    = new StringWriter()
    val pemWriter = new PemWriter(writer)
    pemWriter.writeObject(new PemObject("PUBLIC KEY", publicKey.getEncoded))
    pemWriter.close()
    writer.toString
  }
}
