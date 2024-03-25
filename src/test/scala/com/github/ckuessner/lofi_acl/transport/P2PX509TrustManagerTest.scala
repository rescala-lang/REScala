package com.github.ckuessner.lofi_acl.transport

import com.github.ckuessner.lofi_acl.crypto.X509Util.toJavaCertificate
import com.github.ckuessner.lofi_acl.crypto.{Ed25519Util, X509TestHelper}
import munit.FunSuite

import java.security.cert.{CertificateException, X509Certificate}

class P2PX509TrustManagerTest extends FunSuite {
  private val uut         = new P2PX509TrustManager()
  private val idKeyPair   = Ed25519Util.generateNewKeyPair
  private val certKeyPair = Ed25519Util.generateNewKeyPair

  test("rejects empty certificate chains") {
    intercept[CertificateException](uut.checkClientTrusted(Array.empty, "UNKOWN"))
    intercept[CertificateException](uut.checkServerTrusted(Array.empty, "UNKOWN"))
  }

  test("P2P certificate is rejected if signature is not signed by identity") {
    val cert = X509TestHelper.genCertSignedByWrongKey(idKeyPair, certKeyPair).toJavaCertificate
    intercept[CertificateException] {
      uut.checkClientTrusted(Array(cert), "UNKNOWN")
    }
    intercept[CertificateException] {
      uut.checkServerTrusted(Array(cert), "UNKNOWN")
    }
  }

  test("P2P certificate is rejected if RDN is not UNIQUE_ID") {
    val cert = X509TestHelper.genCertWithoutUniqueId(idKeyPair, certKeyPair).toJavaCertificate
    intercept[CertificateException] {
      uut.checkClientTrusted(Array(cert), "UNKNOWN")
    }
    intercept[CertificateException] {
      uut.checkServerTrusted(Array(cert), "UNKNOWN")
    }
  }

  test("P2P certificate is rejected if expired") {
    val cert = X509TestHelper.genExpiredCert(idKeyPair, certKeyPair).toJavaCertificate
    intercept[CertificateException] {
      uut.checkClientTrusted(Array(cert), "UNKNOWN")
    }
    intercept[CertificateException] {
      uut.checkServerTrusted(Array(cert), "UNKNOWN")
    }
  }
}
