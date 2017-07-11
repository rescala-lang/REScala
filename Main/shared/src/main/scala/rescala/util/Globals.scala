package rescala.util

import java.util.concurrent.ThreadLocalRandom

import scala.util.DynamicVariable
import scala.language.implicitConversions

/**
  * Provides names for dynamic dependencies based on their definition position to allow easier debugging
  */
object Globals {
  def declarationLocationName(): String =
    if (dynamicNameVar.value.nonEmpty) dynamicNameVar.value
    else {
      val trace = Thread.currentThread().getStackTrace
      var i = 0
      while (trace(i).toString.startsWith("scala.") || trace(i).toString.startsWith("java.") ||
        (trace(i).toString.startsWith("rescala.") && !trace(i).toString.startsWith("rescala.test."))) i += 1

      s"${trace(i).getFileName}(${trace(i).getLineNumber})"
    }


  val dynamicNameVar = new DynamicVariable("")
  def named[S](n: String)(f: => S): S = dynamicNameVar.withValue(n)(f)

  def nextID(): Long = ThreadLocalRandom.current().nextLong()
}

trait REName {
  def name: String
}

object REName extends LowPriorityREName {
//  implicit def fromCreation[S <: Struct](implicit ct: CreationTicket[S]): REName = ct.rename
  implicit def fromString(s: String): REName = new REName {
    override def name: String = s
  }
}

trait LowPriorityREName {
  implicit def create(implicit file: sourcecode.Enclosing, line: sourcecode.Line): REName = new REName {
    override def name: String = s"${file.value}:${line.value}"
  }
}
