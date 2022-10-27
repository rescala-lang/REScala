package com.github.ckuessner.encrdt.crdts.interfaces

trait SetCrdt[T] {
  def add(element: T): Unit

  def addAll(iterable: Iterable[T]): Unit = {
    iterable.foreach(add)
  }

  def remove(element: T): Unit

  def removeAll(iterable: Iterable[T]): Unit = {
    iterable.foreach(remove)
  }

  def values: Set[T]
}
