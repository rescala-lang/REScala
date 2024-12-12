package lofi_acl.crypto

import java.nio.charset.StandardCharsets

case class CertificatePem(pemString: String) {
  def getBytes: Array[Byte] = pemString.getBytes(StandardCharsets.UTF_8)
}
