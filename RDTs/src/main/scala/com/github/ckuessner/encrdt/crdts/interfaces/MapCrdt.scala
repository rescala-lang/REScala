package com.github.ckuessner.encrdt.crdts.interfaces

trait MapCrdt[K, V] {
  inline def apply(key: K): Option[V] = get(key)

  def get(key: K): Option[V]

  // No return value, as effect is only eventually consistent
  def put(key: K, value: V): Unit

  // No return value, as effect is only eventually consistent
  def remove(key: K): Unit

  def values: Map[K, V]
}
