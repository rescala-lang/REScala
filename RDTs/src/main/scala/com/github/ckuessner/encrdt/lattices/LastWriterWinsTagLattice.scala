package com.github.ckuessner.encrdt.lattices

import java.time.Instant

object LastWriterWinsTagLattice {
  implicit def lwwLattice: SemiLattice[(Instant, String)] = (left, right) =>
    if (left == right) left
    else if (left._1.isAfter(right._1)) left
    else if (right._1.isAfter(left._1)) right
    else if (left._2 > right._2) left
    else if (right._2 > left._2) right
    else throw new IllegalArgumentException()
}
