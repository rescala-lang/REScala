package de.ckuessner
package encrdt.util

object MapHelper {
  def max[K](a: Map[K, Long], b: Map[K, Long]): Map[K, Long] =
    (a.keySet ++ b.keySet)
      .map(key => key -> a.getOrElse(key, 0).max(b.getOrElse(key, 0)))
      .toMap

  def max[K](a: Map[K, Int], b: Map[K, Int]): Map[K, Int] =
    (a.keySet ++ b.keySet)
      .map(key => key -> a.getOrElse(key, 0).max(b.getOrElse(key, 0)))
      .toMap
}
