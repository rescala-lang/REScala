package com.github.ckuessner.encrdt.crdts

import com.github.ckuessner.encrdt.crdts.interfaces.SetCrdt

class GrowOnlySet[T] extends SetCrdt[T] {
  private var _state: Set[T] = Set()

  def state: Set[T] = _state

  override def add(element: T): Unit = _state += element

  override def remove(element: T): Unit =
    throw new UnsupportedOperationException("Can't remove elements from GrowOnlySet")

  override def values: Set[T] = _state
}
