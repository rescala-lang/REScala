package loci
package communicator

import java.security.cert.{Certificate, X509Certificate}

trait Protocol {
  final def get[T: ProtocolInfo]: Option[T] =
    implicitly[ProtocolInfo[T]].apply(this)
}

trait ProtocolInfo[T] {
  def apply(protocol: Protocol): Option[T]
}


trait SetupInfo { this: Protocol =>
  val setup: ConnectionSetup[_ <: ProtocolCommon]
  final def setupBy(setup: ConnectionSetup[_ <: ProtocolCommon]): Boolean =
    setup eq this.setup
}

object SetupInfo {
  def unapply(info: SetupInfo) = Some(info.setup)

  implicit object info extends ProtocolInfo[SetupInfo] {
    def apply(protocol: Protocol) = protocol match {
      case info: SetupInfo => Some(info)
      case _ => None
    }
  }
}


trait RequestInfo { this: Protocol =>
  val request: Any
}

object RequestInfo {
  def unapply(info: RequestInfo) = Some(info.request)

  implicit object info extends ProtocolInfo[RequestInfo] {
    def apply(protocol: Protocol) = protocol match {
      case info: RequestInfo => Some(info)
      case _ => None
    }
  }
}


trait ResponseInfo { this: Protocol =>
  val response: Any
}

object ResponseInfo {
  def unapply(info: ResponseInfo) = Some(info.response)

  implicit object info extends ProtocolInfo[ResponseInfo] {
    def apply(protocol: Protocol) = protocol match {
      case info: ResponseInfo => Some(info)
      case _ => None
    }
  }
}


trait SecurityInfo { this: Protocol =>
  val encrypted: Boolean
  val integrityProtected: Boolean
  val authenticated: Boolean
}

object SecurityInfo {
  def unapply(info: SecurityInfo) =
    Some((info.encrypted, info.integrityProtected, info.authenticated))

  implicit object info extends ProtocolInfo[SecurityInfo] {
    def apply(protocol: Protocol) = protocol match {
      case info: SecurityInfo => Some(info)
      case _ => None
    }
  }
}

trait Secure { this: Protocol with SecurityInfo =>
  final val encrypted = true
  final val integrityProtected = true
}


trait AuthenticationInfo { this: Protocol with SecurityInfo => }

object AuthenticationInfo {
  def unapply(info: AuthenticationInfo) = true

  implicit object info extends ProtocolInfo[AuthenticationInfo] {
    def apply(protocol: Protocol) = protocol match {
      case info: AuthenticationInfo => Some(info)
      case _ => None
    }
  }
}

trait NameAuthentication extends AuthenticationInfo { this: Protocol with SecurityInfo =>
  val name: String
}

object NameAuthentication {
  def unapply(authentication: NameAuthentication) =
    Some(authentication.name)

  implicit object info extends ProtocolInfo[NameAuthentication] {
    def apply(protocol: Protocol) = protocol match {
      case info: NameAuthentication => Some(info)
      case _ => None
    }
  }
}

trait CertificateAuthentication extends AuthenticationInfo { this: Protocol with SecurityInfo =>
  val certificates: Seq[Certificate]
}

object CertificateAuthentication {
  def unapply(authentication: CertificateAuthentication) =
    Some(authentication.certificates)

  implicit object info extends ProtocolInfo[CertificateAuthentication] {
    def apply(protocol: Protocol) = protocol match {
      case info: CertificateAuthentication => Some(info)
      case _ => None
    }
  }
}

trait X509CertificateAuthentication extends CertificateAuthentication { this: Protocol with SecurityInfo =>
  val certificates: Seq[X509Certificate]
}

object X509CertificateAuthentication {
  def unapply(authentication: X509CertificateAuthentication) =
    Some(authentication.certificates)

  implicit object info extends ProtocolInfo[X509CertificateAuthentication] {
    def apply(protocol: Protocol) = protocol match {
      case info: X509CertificateAuthentication => Some(info)
      case _ => None
    }
  }
}


trait SymmetryInfo { this: Protocol =>
  val symmetry: Symmetry
}

object SymmetryInfo {
  def unapply(info: SymmetryInfo) = Some(info.symmetry)

  implicit object info extends ProtocolInfo[SymmetryInfo] {
    def apply(protocol: Protocol) = protocol match {
      case info: SymmetryInfo => Some(info)
      case _ => None
    }
  }
}

sealed trait Symmetry

object Symmetry {
  case object Inbound extends Symmetry
  case object Outbound extends Symmetry
  case object Bidirectional extends Symmetry
  case object Request extends Symmetry
  case object Response extends Symmetry
}

trait Inbound { this: Protocol with SymmetryInfo =>
  final val symmetry = Symmetry.Inbound
}

trait Outbound { this: Protocol with SymmetryInfo =>
  final val symmetry = Symmetry.Outbound
}

trait Bidirectional { this: Protocol with SymmetryInfo =>
  final val symmetry = Symmetry.Bidirectional
}

trait Request { this: Protocol with SymmetryInfo =>
  final val symmetry = Symmetry.Request
}

trait Response { this: Protocol with SymmetryInfo =>
  final val symmetry = Symmetry.Response
}
