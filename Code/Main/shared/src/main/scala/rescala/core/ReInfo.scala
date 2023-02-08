package rescala.core

/** Provides names for dynamic dependencies based on their definition position to allow easier debugging */
case class ReInfo(str: String) {
  def derive(derivation: String): ReInfo = s"»$str«'$derivation"
}

abstract class ReNamed(val rename: ReInfo) {
  override def toString: String = rename.str
}

//  implicit def fromCreation[S : Struct](implicit ct: CreationTicket[S]): ReName = ct.rename
object ReInfo extends LowPriorityReName {
  implicit def fromString(s: String): ReInfo                = ReInfo(s)
  def named[T](name: String)(f: /* implicit */ ReInfo => T) = f(ReInfo(name))

  private var seenNames = Map[ReInfo, Int]()
  def makeNameUnique(name: ReInfo): ReInfo =
    synchronized {
      val count = seenNames.getOrElse(name, 0)
      seenNames = seenNames.updated(name, count + 1)
      if (count != 0) name.derive(count.toString) else name
    }

}

trait LowPriorityReName {
  implicit def create(implicit file: sourcecode.Enclosing, line: sourcecode.Line): ReInfo =
    ReInfo.makeNameUnique(ReInfo(s"${file.value}:${line.value}"))
}
