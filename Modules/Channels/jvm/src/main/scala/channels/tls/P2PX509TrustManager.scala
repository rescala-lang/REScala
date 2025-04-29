package channels.tls

import java.security.cert.{CertificateException, X509Certificate}
import javax.net.ssl.X509TrustManager

class P2PX509TrustManager extends X509TrustManager {

  /** Verifies that the certificate is signed by subject unique identifier of self-signed certificate. */
  private def verifyP2PCertificate(certificate: X509Certificate): Unit = {
    if "Ed25519" != certificate.getSigAlgName then
      throw CertificateException("Only Ed25519 is supported as signature algo")
    if certificate.getVersion != 3 then throw CertificateException("Only X509v3 Certificates are supported")

    // Validates and verifies the certificate
    val id = X509Util.certificateToPublicIdentity(certificate)
  }

  override def checkClientTrusted(chain: Array[X509Certificate], authType: String): Unit = {
    if chain.length != 1 then {
      throw CertificateException("Only single entry certificate chain is supported")
    }

    verifyP2PCertificate(chain(0))
  }

  override def checkServerTrusted(chain: Array[X509Certificate], authType: String): Unit = {
    if chain.length != 1 then {
      throw CertificateException("Only single entry certificate chain is supported")
    }

    verifyP2PCertificate(chain(0))
  }

  override def getAcceptedIssuers: Array[X509Certificate] = Array.empty
}
