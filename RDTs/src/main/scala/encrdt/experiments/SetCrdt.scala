package de.ckuessner
package encrdt.experiments

trait SetCrdt[T] {
  def add(element: T)

  def addAll(iterable: Iterable[T]): Unit = {
    iterable.foreach(add)
  }

  def remove(element: T)

  def removeAll(iterable: Iterable[T]): Unit = {
    iterable.foreach(remove)
  }

  def values: Set[T]
}
