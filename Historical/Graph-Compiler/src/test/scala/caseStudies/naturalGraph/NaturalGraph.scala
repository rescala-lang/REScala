package caseStudies.naturalGraph

import api2.*
import api2.given

object NaturalGraph {
  CompileGraph.isolated("naturalGraph") {
    val source = CSignal.source[Int](0)

    // row 3
    val c1 = source.map(_ + 1)

    // row 2
    val b1 = c1.map(_ + 1)
    val b2 = b1.map(_ + 1)
    val b3 = b2.map(_ + 1)

    // row 3
    val c2 = b3.map(_ + 1)
    val c3 = CSignal { c2.value; 0 }
    val c4 = c3.map(_ + 1)

    // row 1
    val a1 = b2.map(_ + 1)
    val a2 = a1.map(_ + 1)
    val a3 = CSignal { a2.value + b2.value }
    val a4 = a3.map(_ + 1)

    // row2
    val b4 = CSignal { a4.value; b3.value; 0 }
    val b5 = b4.map(_ + 1)
    val b6 = b5.map(_ + 1)
    val b7 = b6.map(_ + 1)
    val b8 = CSignal { b7.value + c2.value }

    // row 3
    val c5 = CSignal { c4.value + b8.value }

    // row 4
    val d1 = c2.map(_ + 1)

    // row 5
    val e1 = CSignal { c1.value; 0 }
    val e2 = e1.map(_ + 1)
    val e3 = e2.map(_ + 1)
    val e4 = e3.map(_ + 1)
    val e5 = CSignal { e4.value + c2.value }

    val e6 = c2.map(_ + 1)
    val e7 = CSignal { e6.value + d1.value }

    val result = CSignal { (c5.value, e5.value, e7.value) }
  }
}
