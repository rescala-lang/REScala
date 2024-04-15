package com.github.ckuessner.lofi_acl.transport

import com.github.ckuessner.lofi_acl.crypto.X509Util

import java.security.cert.{CertificateException, X509Certificate}
import javax.net.ssl.X509TrustManager

class P2PX509TrustManager extends X509TrustManager {

  /** Verifies that the certificate is signed by subject unique identifier of self-signed certificate.
    */
  private def verifyP2PCertificate(certificate: X509Certificate): Unit = {
    if ("Ed25519" != certificate.getSigAlgName)
      throw CertificateException("Only Ed25519 is supported as signature algo")
    if (certificate.getVersion != 3) throw CertificateException("Only X509v3 Certificates are supported")

    // Certificate still valid?
    certificate.checkValidity()

    // Self issued?
    val subject = certificate.getSubjectX500Principal
    val issuer  = certificate.getIssuerX500Principal
    if (!subject.equals(issuer)) throw CertificateException("certificate not self-issued")

    // Verifies that certificate is actually signed by identity in certificate
    val id = X509Util.certificateToPublicIdentity(certificate)
  }

  override def checkClientTrusted(chain: Array[X509Certificate], authType: String): Unit = {
    if (chain.length != 1) {
      throw CertificateException("Only single entry certificate chain is supported")
    }

    verifyP2PCertificate(chain(0))
  }

  override def checkServerTrusted(chain: Array[X509Certificate], authType: String): Unit = {
    if (chain.length != 1) {
      throw CertificateException("Only single entry certificate chain is supported")
    }

    verifyP2PCertificate(chain(0))
  }

  override def getAcceptedIssuers: Array[X509Certificate] = Array.empty
}
