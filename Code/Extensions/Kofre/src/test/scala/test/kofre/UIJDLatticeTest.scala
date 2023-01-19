package test.kofre

import kofre.base.Lattice.{*, given}
import kofre.base.Lattice
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}

class IntAsLatticeTest extends munit.ScalaCheckSuite {
  property("leq") {
    forAll { (a: Int, b: Int, c: Int) =>
      assert(
        summon[Lattice[Int]].lteq(a, a),
        s"leq should be reflexive, but $a is not leq $a"
        )

      assert(
        !(summon[Lattice[Int]].lteq(a, b) && summon[Lattice[Int]].lteq(b, c)) || summon[Lattice[Int]].lteq(a, c),
        s"leq should be transitive, but $a leq $b and $b leq $c and $a is not leq $c"
      )
    }
  }

  property("merge") {
    forAll { (a: Int, b: Int, c: Int) =>
      val mergeAB    = summon[Lattice[Int]].merge(a, b)
      val mergeTwice = summon[Lattice[Int]].merge(mergeAB, b)

      assertEquals(
        mergeTwice,
        mergeAB,
        s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
      )

      val mergeBA = summon[Lattice[Int]].merge(b, a)

      assertEquals(
        mergeBA,
        mergeAB,
        s"merge should be commutative, but $mergeBA does not equal $mergeAB"
      )

      val mergeAB_C = summon[Lattice[Int]].merge(mergeAB, c)
      val mergeBC   = summon[Lattice[Int]].merge(b, c)
      val mergeA_BC = summon[Lattice[Int]].merge(a, mergeBC)

      assertEquals(
        mergeA_BC,
        mergeAB_C,
        s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
      )
    }
  }

  property("decompose") {
    forAll { (i: Int) =>
      val decomposed = summon[Lattice[Int]].decompose(i)
      val merged     = decomposed.reduceOption(summon[Lattice[Int]].merge)

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

class SetAsLatticeTest extends munit.ScalaCheckSuite {
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

class OptionAsLatticeTest extends munit.ScalaCheckSuite {
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

class MapAsLatticeTest extends munit.ScalaCheckSuite {
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

class PairAsLatticeTest extends munit.ScalaCheckSuite {
  property("leq") {
    forAll { (a: (Set[Int], Set[Int]), b: (Set[Int], Set[Int]), c: (Set[Int], Set[Int])) =>
      assert(
        Lattice[(Set[Int], Set[Int])].lteq(a, a),
        s"leq should be reflexive, but $a is not leq $a"
      )

      assert(
        !(Lattice[(Set[Int], Set[Int])].lteq(a, b) && Lattice[(Set[Int], Set[Int])].lteq(
          b,
          c
        )) || Lattice[(Set[Int], Set[Int])].lteq(a, c),
        s"leq should be transitive, but $a leq $b and $b leq $c and $a is not leq $c"
      )
    }
  }

  property("merge") {
    forAll { (a: (Set[Int], Set[Int]), b: (Set[Int], Set[Int]), c: (Set[Int], Set[Int])) =>
      val mergeAB    = Lattice[(Set[Int], Set[Int])].merge(a, b)
      val mergeTwice = Lattice[(Set[Int], Set[Int])].merge(mergeAB, b)

      assertEquals(
        mergeTwice,
        mergeAB,
        s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
      )

      val mergeBA = Lattice[(Set[Int], Set[Int])].merge(b, a)

      assertEquals(
        mergeBA,
        mergeAB,
        s"merge should be commutative, but $mergeBA does not equal $mergeAB"
      )

      val mergeAB_C = Lattice[(Set[Int], Set[Int])].merge(mergeAB, c)
      val mergeBC   = Lattice[(Set[Int], Set[Int])].merge(b, c)
      val mergeA_BC = Lattice[(Set[Int], Set[Int])].merge(a, mergeBC)

      assertEquals(
        mergeA_BC,
        mergeAB_C,
        s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
      )
    }
  }

  property("decompose") {
    forAll { (p: (Set[Int], Set[Int])) =>
      val decomposed = Lattice[(Set[Int], Set[Int])].decompose(p)
      val merged     = decomposed.reduceOption(Lattice[(Set[Int], Set[Int])].merge)

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
