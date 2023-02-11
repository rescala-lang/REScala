package compiler.context

import scala.collection.mutable

class MappingLabel[K, V](register: V => Unit = (_: V) => ()) {
  private val map: mutable.Map[K, V] = mutable.Map()

  export map.{get, contains, remove, apply, keys, keySet, values, clear}

  def put(key: K, value: V): V = {
    map.put(key, value)
    register(value)
    value
  }

  def getOrElseUpdate(key: K, value: => V): V = {
    map.get(key) match {
      case Some(v) => v
      case None    => put(key, value)
    }
  }
}
