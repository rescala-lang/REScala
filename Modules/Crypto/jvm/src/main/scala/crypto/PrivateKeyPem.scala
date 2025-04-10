package crypto

import java.nio.charset.StandardCharsets

case class PrivateKeyPem(pemString: String) {
  def getBytes: Array[Byte] = pemString.getBytes(StandardCharsets.UTF_8)
}
