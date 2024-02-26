package reactives.core

/** Provides names for dynamic dependencies based on their definition position to allow easier debugging */
case class ReInfo(idCounter: Int, description: String, enclosing: String, file: String, line: Int) {
  def derive(derivation: String): ReInfo = copy(description = s"»$description«'$derivation")
}

object ReInfo {

  def named[T](name: String)(f: /* implicit */ ReInfo => T)(implicit info: ReInfo) = f(info.derive(name))

  implicit def create(implicit
      file: sourcecode.File,
      enclosing: sourcecode.Enclosing,
      line: sourcecode.Line
  ): ReInfo = ReInfo(nextCount(), "", enclosing.value.intern(), file.value.intern(), line.value)

  private var counter: Int = 0
  private def nextCount() = synchronized {
    counter = counter + 1
    counter
  }

}
