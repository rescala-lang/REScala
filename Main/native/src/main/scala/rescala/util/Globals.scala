package rescala.util

import scala.util.DynamicVariable

/**
  * Provides names for dynamic dependencies based on their definition position to allow easier debugging
  */
object Globals {
  def declarationLocationName(): String = dynamicNameVar.value


  val dynamicNameVar = new DynamicVariable("Unnamed")
  def named[S](n: String)(f: => S): S = dynamicNameVar.withValue(n)(f)

  def nextID(): Long = scala.util.Random.nextLong()
}
