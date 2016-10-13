package rescala.util

object WeakHashMap {
  def empty[K, V]: scala.collection.mutable.HashMap[K, V] = scala.collection.mutable.HashMap.empty[K, V]
}
