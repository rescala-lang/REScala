package rescala.core

/**
  * Provides names for dynamic dependencies based on their definition position to allow easier debugging
  */
case class REName(str: String) {
  def derive(derivation: String): REName = s"»$str«'$derivation"
}

abstract class RENamed(val rename: REName) {
  override def toString: String = rename.str
}

//  implicit def fromCreation[S <: Struct](implicit ct: CreationTicket[S]): REName = ct.rename
object REName extends LowPriorityREName {
  implicit def fromString(s: String): REName = REName(s)
  def named[T](name: String)(f: /* implicit */ REName => T) = f(REName(name))


  private var seenNames = Map[REName, Int]()
  def makeNameUnique(name: REName): REName = synchronized {
    val count =  seenNames.getOrElse(name, 0)
    seenNames = seenNames.updated(name, count + 1)
    if (count != 0) name.derive(count.toString) else name
  }


}

trait LowPriorityREName {
  implicit def create(implicit file: sourcecode.Enclosing, line: sourcecode.Line): REName =
    REName.makeNameUnique(REName(s"${file.value}:${line.value}"))
}

