package loci
package utility

import scala.reflect.macros.Universe

object noReporting {
  def apply[T](universe: Universe, default: T)(body: => T) = {
    val reset = try {
      val reporterClass = Class.forName("scala.tools.nsc.reporters.Reporter")
      val storeReporterClass = Class.forName("scala.tools.nsc.reporters.StoreReporter")

      val getRepoter = universe.getClass.getMethod("reporter")
      val setRepoter = universe.getClass.getMethod("reporter_$eq", reporterClass)

      val reporter = getRepoter.invoke(universe)

      storeReporterClass.getConstructor().newInstance() match {
        case storeReporter: AnyRef =>
          setRepoter.invoke(universe, storeReporter)
          Some(setRepoter -> reporter)
        case _ =>
          None
      }
    }
    catch {
      case _: ClassNotFoundException | _: NoSuchMethodException |  _: IllegalArgumentException =>
        None
    }

    reset.fold(default) { case (setRepoter, reporter) =>
      try body
      finally {
        try setRepoter.invoke(universe, reporter)
        catch { case  _: IllegalArgumentException => }
      }
    }
  }
}
