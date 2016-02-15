package rescala.graph

import java.util.concurrent.ThreadLocalRandom

import scala.util.DynamicVariable

/** support for dynamic dependency discovery */
object Globals {
  def declarationLocationName() =
    if (dynamicNameVar.value.nonEmpty) dynamicNameVar.value
    else {
      val trace = Thread.currentThread().getStackTrace
      var i = 0
      while (trace(i).toString.startsWith("scala.") || trace(i).toString.startsWith("java.") ||
        (trace(i).toString.startsWith("rescala.") && !trace(i).toString.startsWith("rescala.test."))) i += 1

      s"${ trace(i).getFileName }(${ trace(i).getLineNumber })"
    }


  val dynamicNameVar = new DynamicVariable("")
  def named[S](n: String)(f: => S): S = dynamicNameVar.withValue(n)(f)

  def nextID() = ThreadLocalRandom.current().nextLong()
}
