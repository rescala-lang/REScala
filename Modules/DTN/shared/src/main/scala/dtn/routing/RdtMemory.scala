package dtn.routing

import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet

object BundleIdAgeOrdering extends Ordering[String] {
  def compare(x: String, y: String): Int = {
    val Array(x_count, x_time) = x.split("-").reverse.take(2)
    val Array(y_count, y_time) = y.split("-").reverse.take(2)

    val res = x_time.toInt - y_time.toInt

    if res == 0 then x_count.toInt - y_count.toInt else res
  }
}

object GarbageCollector {
  val SOFT_MAX_BUNDLE_COUNT = 20

  var sortedIds: SortedSet[String] = TreeSet[String]()(using BundleIdAgeOrdering)

  def shouldKeep(bundleId: String): Boolean = {
    sortedIds = sortedIds + bundleId

    return !(sortedIds.size > SOFT_MAX_BUNDLE_COUNT && sortedIds.head == bundleId)
  }
}
