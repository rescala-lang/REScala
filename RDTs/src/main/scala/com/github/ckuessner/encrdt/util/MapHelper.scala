package com.github.ckuessner.encrdt.util

object MapHelper {

  /** Returns map where each value is the maximum in both maps. If a key is present in one map, takes max(0, val).
    *
    * @param a
    *   map a
    * @param b
    *   map b
    * @tparam K
    *   Type of key
    * @tparam V
    *   Type of value
    * @return
    *   Map containing all keys of both maps with maximum value per key
    */
  def max[K, V](a: Map[K, V], b: Map[K, V])(implicit num: Numeric[V]): Map[K, V] =
    (a.keySet ++ b.keySet)
      .map(key => key -> num.max(a.getOrElse(key, num.zero), b.getOrElse(key, num.zero)))
      .toMap
}
