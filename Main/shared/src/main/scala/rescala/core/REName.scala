package rescala.core

import scala.language.implicitConversions

/**
  * Provides names for dynamic dependencies based on their definition position to allow easier debugging
  */
trait REName {
  def name: String
}

abstract class RENamed(rename: REName) {
  override def toString: String = rename.name
}

//  implicit def fromCreation[S <: Struct](implicit ct: CreationTicket[S]): REName = ct.rename
object REName extends LowPriorityREName {
  implicit def fromString(s: String): REName = new REName {
    override def name: String = s
  }
}

trait LowPriorityREName {
  implicit def create(implicit file: sourcecode.Enclosing, line: sourcecode.Line): REName = new REName {
    override def name: String = s"${file.value}:${line.value}"
  }
}

