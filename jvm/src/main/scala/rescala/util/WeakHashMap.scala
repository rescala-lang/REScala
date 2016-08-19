package rescala.util

object WeakHashMap {
  def empty[K, V]: scala.collection.mutable.WeakHashMap[K, V] = scala.collection.mutable.WeakHashMap.empty[K, V]
}
