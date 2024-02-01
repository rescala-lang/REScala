package loci
package communicator
package ws.webnative

import java.security.cert.Certificate

import scala.util.{Failure, Success, Try}

private sealed trait WSProtocolFactory[P <: WS] {
  def make(url: String, host: Option[String], port: Option[Int],
    setup: ConnectionSetup[P], authenticated: Boolean,
    encrypted: Boolean, integrityProtected: Boolean,
    authentication: Seq[Certificate] = Seq.empty): Try[P]
}

private object WSProtocolFactory {
  locally(WSProtocolFactory)

  implicit object ws extends WSProtocolFactory[WS] {
    def make(url: String, host: Option[String], port: Option[Int],
        setup: ConnectionSetup[WS], authenticated: Boolean,
        encrypted: Boolean, integrityProtected: Boolean,
        certificates: Seq[Certificate]): Try[WS] =
      Success(construct(
        url, host, port, setup, authenticated,
        encrypted, integrityProtected, certificates))
  }

  implicit object wsSecure extends WSProtocolFactory[WS.Secure] {
    def make(url: String, host: Option[String], port: Option[Int],
        setup: ConnectionSetup[WS.Secure], authenticated: Boolean,
        encrypted: Boolean, integrityProtected: Boolean,
        certificates: Seq[Certificate]): Try[WS.Secure] =
      construct(
          url, host, port, setup, authenticated,
          encrypted, integrityProtected, certificates) match {
        case ws: WS.Secure => Success(ws)
        case _ => Failure(new ConnectionException("connection not secure"))
      }
  }

  private def construct(
      _url: String, _host: Option[String], _port: Option[Int],
      _setup: ConnectionSetup[WS], _authenticated: Boolean,
      _encrypted: Boolean, _integrityProtected: Boolean,
      _certificates: Seq[Certificate]) =
    if (_encrypted && _integrityProtected) {
      if (_certificates.isEmpty)
        new WS.Secure {
          val url = _url; val host = _host; val port = _port
          val setup = _setup; val authenticated = _authenticated
        }
      else
        new WS.Secure with CertificateAuthentication {
          val url = _url; val host = _host; val port = _port
          val setup = _setup; val authenticated = _authenticated
          val certificates = _certificates
        }
    }
    else {
      if (_certificates.isEmpty)
        new WS {
          val url = _url; val host = _host; val port = _port
          val setup = _setup; val authenticated = _authenticated
          val encrypted = _encrypted
          val integrityProtected = _integrityProtected
        }
      else
        new WS with CertificateAuthentication {
          val url = _url; val host = _host; val port = _port
          val setup = _setup; val authenticated = _authenticated
          val encrypted = _encrypted
          val integrityProtected = _integrityProtected
          val certificates = _certificates
        }
    }
}
