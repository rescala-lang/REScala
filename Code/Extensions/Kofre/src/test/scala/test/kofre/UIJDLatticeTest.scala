package test.kofre

import kofre.base.DecomposeLattice.{*, given}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}

class IntAsDecomposeLatticeTest extends munit.ScalaCheckSuite {
  property("leq") {
    forAll { (a: Int, b: Int, c: Int) =>
      assert(
        intMaxLattice.lteq(a, a),
        s"leq should be reflexive, but $a is not leq $a"
      )

      assert(
        !(intMaxLattice.lteq(a, b) && intMaxLattice.lteq(b, c)) || intMaxLattice.lteq(a, c),
        s"leq should be transitive, but $a leq $b and $b leq $c and $a is not leq $c"
      )
    }
  }

  property("merge") {
    forAll { (a: Int, b: Int, c: Int) =>
      val mergeAB    = intMaxLattice.merge(a, b)
      val mergeTwice = intMaxLattice.merge(mergeAB, b)

      assertEquals(
        mergeTwice,
        mergeAB,
        s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
      )

      val mergeBA = intMaxLattice.merge(b, a)

      assertEquals(
        mergeBA,
        mergeAB,
        s"merge should be commutative, but $mergeBA does not equal $mergeAB"
      )

      val mergeAB_C = intMaxLattice.merge(mergeAB, c)
      val mergeBC   = intMaxLattice.merge(b, c)
      val mergeA_BC = intMaxLattice.merge(a, mergeBC)

      assertEquals(
        mergeA_BC,
        mergeAB_C,
        s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
      )
    }
  }

  property("decompose") {
    forAll { (i: Int) =>
      val decomposed = intMaxLattice.decompose(i)
      val merged     = decomposed.reduceOption(intMaxLattice.merge)

      merged match {
        case None =>
        case Some(n) =>
          assertEquals(
            n,
            i,
            s"merging the atoms produced by decompose should yield the original value, but $n does not equal $i"
          )
      }
    }
  }
}

class SetAsDecomposeLatticeTest extends munit.ScalaCheckSuite {
  property("leq") {
    forAll { (a: Set[Int], b: Set[Int], c: Set[Int]) =>
      assert(
        setLattice.lteq(a, a),
        s"leq should be reflexive, but $a is not leq $a"
      )

      assert(
        !(setLattice.lteq(a, b) && setLattice.lteq(b, c)) || setLattice.lteq(a, c),
        s"leq should be transitive, but $a leq $b and $b leq $c and $a is not leq $c"
      )
    }
  }

  property("merge") {
    forAll { (a: Set[Int], b: Set[Int], c: Set[Int]) =>
      val mergeAB    = setLattice.merge(a, b)
      val mergeTwice = setLattice.merge(mergeAB, b)

      assertEquals(
        mergeTwice,
        mergeAB,
        s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
      )

      val mergeBA = setLattice.merge(b, a)

      assertEquals(
        mergeBA,
        mergeAB,
        s"merge should be commutative, but $mergeBA does not equal $mergeAB"
      )

      val mergeAB_C = setLattice.merge(mergeAB, c)
      val mergeBC   = setLattice.merge(b, c)
      val mergeA_BC = setLattice.merge(a, mergeBC)

      assertEquals(
        mergeA_BC,
        mergeAB_C,
        s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
      )
    }
  }

  property("decompose") {
    forAll { (s: Set[Int]) =>
      val decomposed = setLattice.decompose(s)
      val merged     = decomposed.reduceOption(setLattice[Int].merge)

      merged match {
        case None =>
        case Some(x) =>
          assertEquals(
            x,
            s,
            s"merging the atoms produced by decompose should yield the original value, but $x does not equal $s"
          )
      }
    }
  }
}

class OptionAsDecomposeLatticeTest extends munit.ScalaCheckSuite {
  property("leq") {
    forAll { (a: Option[Int], b: Option[Int], c: Option[Int]) =>
      assert(
        optionLattice[Int].lteq(a, a),
        s"leq should be reflexive, but $a is not leq $a"
      )

      assert(
        !(optionLattice[Int].lteq(a, b) && optionLattice[Int].lteq(b, c)) || optionLattice[Int].lteq(
          a,
          c
        ),
        s"leq should be transitive, but $a leq $b and $b leq $c and $a is not leq $c"
      )
    }
  }

  property("merge") {
    forAll { (a: Option[Int], b: Option[Int], c: Option[Int]) =>
      val mergeAB    = optionLattice[Int].merge(a, b)
      val mergeTwice = optionLattice[Int].merge(mergeAB, b)

      assertEquals(
        mergeTwice,
        mergeAB,
        s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
      )

      val mergeBA = optionLattice[Int].merge(b, a)

      assertEquals(
        mergeBA,
        mergeAB,
        s"merge should be commutative, but $mergeBA does not equal $mergeAB"
      )

      val mergeAB_C = optionLattice[Int].merge(mergeAB, c)
      val mergeBC   = optionLattice[Int].merge(b, c)
      val mergeA_BC = optionLattice[Int].merge(a, mergeBC)

      assertEquals(
        mergeA_BC,
        mergeAB_C,
        s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
      )
    }
  }

  property("decompose") {
    forAll { (o: Option[Int]) =>
      val decomposed = optionLattice[Int].decompose(o)
      val merged     = decomposed.reduceOption(optionLattice[Int].merge)

      merged match {
        case None =>
        case Some(x) =>
          assertEquals(
            x,
            o,
            s"merging the atoms produced by decompose should yield the original value, but $x does not equal $o"
          )
      }
    }
  }
}

class MapAsDecomposeLatticeTest extends munit.ScalaCheckSuite {
  property("leq") {
    forAll { (a: Map[Int, Int], b: Map[Int, Int], c: Map[Int, Int]) =>
      assert(
        mapLattice[Int, Int].lteq(a, a),
        s"leq should be reflexive, but $a is not leq $a"
      )

      assert(
        !(mapLattice[Int, Int].lteq(a, b) && mapLattice[Int, Int].lteq(b, c)) || mapLattice[
          Int,
          Int
        ].lteq(
          a,
          c
        ),
        s"leq should be transitive, but $a leq $b and $b leq $c and $a is not leq $c"
      )
    }
  }

  property("merge") {
    forAll { (a: Map[Int, Int], b: Map[Int, Int], c: Map[Int, Int]) =>
      val mergeAB    = mapLattice[Int, Int].merge(a, b)
      val mergeTwice = mapLattice[Int, Int].merge(mergeAB, b)

      assertEquals(
        mergeTwice,
        mergeAB,
        s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
      )

      val mergeBA = mapLattice[Int, Int].merge(b, a)

      assertEquals(
        mergeBA,
        mergeAB,
        s"merge should be commutative, but $mergeBA does not equal $mergeAB"
      )

      val mergeAB_C = mapLattice[Int, Int].merge(mergeAB, c)
      val mergeBC   = mapLattice[Int, Int].merge(b, c)
      val mergeA_BC = mapLattice[Int, Int].merge(a, mergeBC)

      assertEquals(
        mergeA_BC,
        mergeAB_C,
        s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
      )
    }
  }

  property("decompose") {
    forAll { (m: Map[Int, Int]) =>
      val decomposed = mapLattice[Int, Int].decompose(m)
      val merged     = decomposed.reduceOption(mapLattice[Int, Int].merge)

      merged match {
        case None =>
        case Some(x) =>
          assertEquals(
            x,
            m,
            s"merging the atoms produced by decompose should yield the original value, but $x does not equal $m"
          )
      }
    }
  }
}

class PairAsDecomposeLatticeTest extends munit.ScalaCheckSuite {
  property("leq") {
    forAll { (a: (Set[Int], Set[Int]), b: (Set[Int], Set[Int]), c: (Set[Int], Set[Int])) =>
      assert(
        PairAsUIJDLattice[Set[Int], Set[Int]].lteq(a, a),
        s"leq should be reflexive, but $a is not leq $a"
      )

      assert(
        !(PairAsUIJDLattice[Set[Int], Set[Int]].lteq(a, b) && PairAsUIJDLattice[Set[Int], Set[Int]].lteq(
          b,
          c
        )) || PairAsUIJDLattice[Set[Int], Set[Int]].lteq(a, c),
        s"leq should be transitive, but $a leq $b and $b leq $c and $a is not leq $c"
      )
    }
  }

  property("merge") {
    forAll { (a: (Set[Int], Set[Int]), b: (Set[Int], Set[Int]), c: (Set[Int], Set[Int])) =>
      val mergeAB    = PairAsUIJDLattice[Set[Int], Set[Int]].merge(a, b)
      val mergeTwice = PairAsUIJDLattice[Set[Int], Set[Int]].merge(mergeAB, b)

      assertEquals(
        mergeTwice,
        mergeAB,
        s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
      )

      val mergeBA = PairAsUIJDLattice[Set[Int], Set[Int]].merge(b, a)

      assertEquals(
        mergeBA,
        mergeAB,
        s"merge should be commutative, but $mergeBA does not equal $mergeAB"
      )

      val mergeAB_C = PairAsUIJDLattice[Set[Int], Set[Int]].merge(mergeAB, c)
      val mergeBC   = PairAsUIJDLattice[Set[Int], Set[Int]].merge(b, c)
      val mergeA_BC = PairAsUIJDLattice[Set[Int], Set[Int]].merge(a, mergeBC)

      assertEquals(
        mergeA_BC,
        mergeAB_C,
        s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
      )
    }
  }

  property("decompose") {
    forAll { (p: (Set[Int], Set[Int])) =>
      val decomposed = PairAsUIJDLattice[Set[Int], Set[Int]].decompose(p)
      val merged     = decomposed.reduceOption(PairAsUIJDLattice[Set[Int], Set[Int]].merge)

      merged match {
        case None =>
        case Some(x) =>
          assertEquals(
            x,
            p,
            s"merging the atoms produced by decompose should yield the original value, but $x does not equal $p"
          )
      }
    }
  }
}
