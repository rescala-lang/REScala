package com.github.ckuessner.ardt.base

import scala.compiletime.{erasedValue, error, summonAll, summonInline}
import scala.deriving.Mirror

trait Lattice[T] {
  def merge(left: T, right: T): T
}

object Lattice {
  inline def apply[A](using lattice: Lattice[A]): Lattice[A] = lattice

  def merge[A: Lattice](left: A, right: A): A = apply[A].merge(left, right)

  // See: https://docs.scala-lang.org/scala3/reference/contextual/derivation.html
  // and https://github.com/rescala-lang/REScala/blob/master/Modules/RDTs/src/main/scala/rdts/base/Lattice.scala
  inline def derived[T <: Product](using mirror: Mirror.ProductOf[T]): Lattice[T] =
    (left: T, right: T) =>
      val factorLattices = summonAll[Tuple.Map[mirror.MirroredElemTypes, Lattice]]
      val leftAsProd     = left.asInstanceOf[Product]
      val rightAsProd    = right.asInstanceOf[Product]

      mirror.fromProduct(new Product {
        override def canEqual(that: Any): Boolean = false

        override def productArity: Int = factorLattices.size

        override def productElement(n: Int): Any = {
          factorLattices
            .productElement(n)
            .asInstanceOf[Lattice[Any]]
            .merge(leftAsProd.productElement(n), rightAsProd.productElement(n))
        }
      })
}
