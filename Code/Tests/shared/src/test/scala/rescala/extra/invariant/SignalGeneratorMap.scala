package rescala.extra.invariant

import scala.collection.mutable


class SignalGeneratorMap[K[_], +V[_]] private(baseMap: scala.collection.mutable.WeakHashMap[Any, Any]) {
  def apply[T](key: K[T]): V[T] = baseMap(key).asInstanceOf[V[T]]

  def put[T1, T2, V1[X] >: V[X]](key: K[T1], value: V1[T2])(implicit ev: T1 =:= T2): Option[Any] = baseMap.put(key, value)

  def map[U](f: ((Any, Any)) => U): mutable.Iterable[U] = baseMap.map(f)

  def entries(): List[(K[Any], V[Any])] = this.map(pair => pair).toList.asInstanceOf[List[(K[Any], V[Any])]]

}

object SignalGeneratorMap {
  def apply[K[_], V[_]] = new SignalGeneratorMap[K, V](scala.collection.mutable.WeakHashMap.empty[Any, Any])
}
