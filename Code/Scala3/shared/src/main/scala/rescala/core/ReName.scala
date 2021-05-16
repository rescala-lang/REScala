package rescala.core

/** Provides names for dynamic dependencies based on their definition position to allow easier debugging */
case class ReName(str: String) {
  def derive(derivation: String): ReName = s"»$str«'$derivation"
}

abstract class ReNamed(val rename: ReName) {
  override def toString: String = rename.str
}

//  implicit def fromCreation[S : Struct](implicit ct: CreationTicket[S]): ReName = ct.rename
object ReName extends LowPriorityReName {
  implicit def fromString(s: String): ReName                = ReName(s)
  def named[T](name: String)(f: /* implicit */ ReName => T) = f(ReName(name))

  private var seenNames = Map[ReName, Int]()
  def makeNameUnique(name: ReName): ReName =
    synchronized {
      val count = seenNames.getOrElse(name, 0)
      seenNames = seenNames.updated(name, count + 1)
      if (count != 0) name.derive(count.toString) else name
    }

}

trait LowPriorityReName {
  implicit def create(implicit file: sourcecode.Enclosing, line: sourcecode.Line): ReName =
    ReName.makeNameUnique(ReName(s"${file.value}:${line.value}"))
}
