package lofi_acl.crypto

import com.google.crypto.tink.subtle.XChaCha20Poly1305
import lofi_acl.crypto.KeyDerivationKey.*
import org.bouncycastle.crypto.generators.HKDFBytesGenerator
import org.bouncycastle.crypto.params.HKDFParameters
import org.bouncycastle.crypto.util.DigestFactory

import java.nio.charset.StandardCharsets
import java.nio.charset.StandardCharsets.UTF_8
import java.security.{KeyPair, SecureRandom}

class KeyDerivationKey /* private to make sure that ikm is not modified */ private (
    private val inputKeyMaterial: Array[Byte]
) {
  require(inputKeyMaterial.length == IKM_LENGTH)

  lazy val encryptionKey: XChaCha20Poly1305 = {
    XChaCha20Poly1305(
      derive256BitsOutputKeyMaterial(inputKeyMaterial, contextForEncryptionKeys)
    )
  }

  // It would probably be a good idea to separate encryptionKey and signingKey into two different classes, since the
  // type should indicate the use of the KeyDerivationKey (i.e., AeadKeyDerivationKey and SigningKeyDerivationKey).
  lazy val signingKey: Ed25519PrivateKey = {
    Ed25519Util.rawPrivateKeyBytesToKeyPair(
      derive256BitsOutputKeyMaterial(inputKeyMaterial, contextForSigningKeys)
    )
  }

  def childKeyDerivationKey(realm: String): KeyDerivationKey = {
    KeyDerivationKey(generateChildKeyDerivationKeyMaterial(inputKeyMaterial, realm))
  }

  def recursiveChildKeyDerivationKey(path: Array[String]): KeyDerivationKey = {
    require(path.nonEmpty) // Prevents API misuse, don't leak inputKeyMaterial
    val outputKeyMaterial = path.foldLeft(inputKeyMaterial) { (ikm, pathElement) =>
      generateChildKeyDerivationKeyMaterial(inputKeyMaterial, pathElement)
    }
    KeyDerivationKey(outputKeyMaterial)
  }
}

object KeyDerivationKey {
  private val contextForEncryptionKeys: Array[Byte]       = "encryption".getBytes(UTF_8)
  private val contextForSigningKeys: Array[Byte]          = "signing".getBytes(UTF_8)
  private val contextPrefixForDerivationKeys: Array[Byte] = "derivation".getBytes(UTF_8)
  inline val IKM_LENGTH                                   = 32 // 256 bits
  inline val OKM_LENGTH                                   = 32 // 256 bits

  type Ed25519PrivateKey = KeyPair

  private val secureRandom = SecureRandom.getInstanceStrong

  def apply(): KeyDerivationKey = {
    val ikm = Array.ofDim[Byte](IKM_LENGTH)
    secureRandom.nextBytes(ikm)
    KeyDerivationKey(ikm)
  }

  def apply(inputKeyMaterial: Array[Byte]): KeyDerivationKey = {
    new KeyDerivationKey(inputKeyMaterial.clone())
  }

  // WARNING: Only use on uniformly distributed input keying material
  private def derive256BitsOutputKeyMaterial(inputKeyMaterial: Array[Byte], info: Array[Byte]): Array[Byte] = {
    require(inputKeyMaterial.length == IKM_LENGTH)
    // KMAC based KDF could be used instead, but it's not available in BC. Also: BC KMAC implementation uses XOF Mode
    // (which NIST recommends against when used in KDF). Otherwise would be simple to construct (see: NIST
    // SP.800-108r1-upd1)

    // We don't need to extract, since we have uniformly distributed input-key-material
    val hkdfParameters = HKDFParameters.skipExtractParameters(inputKeyMaterial, info)
    val hkdf           = HKDFBytesGenerator(DigestFactory.createSHA256())
    hkdf.init(hkdfParameters)
    val outputKeyMaterial: Array[Byte] = Array.ofDim(OKM_LENGTH)
    hkdf.generateBytes(outputKeyMaterial, 0, OKM_LENGTH)
    outputKeyMaterial
  }

  private def generateChildKeyDerivationKeyMaterial(inputKeyMaterial: Array[Byte], realm: String): Array[Byte] = {
    val context: Array[Byte] = Array.ofDim(contextPrefixForDerivationKeys.length + realm.length)
    val _                    = contextPrefixForDerivationKeys.copyToArray(context)
    val _                    = realm.getBytes(UTF_8).copyToArray(context, contextPrefixForDerivationKeys.length)
    derive256BitsOutputKeyMaterial(inputKeyMaterial, context)
  }

}
