package test.kofre

import kofre.decompose.DecomposeLattice.{*, given}
import kofre.decompose.interfaces.LexCounterInterface.LexPair
import kofre.decompose.interfaces.LexCounterInterface.LexPair.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class IntAsDecomposeLatticeTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  "leq" in forAll { (a: Int, b: Int, c: Int) =>
    assert(
      IntAsUIJDLattice.lteq(a, a),
      s"leq should be reflexive, but $a is not leq $a"
    )

    assert(
      !(IntAsUIJDLattice.lteq(a, b) && IntAsUIJDLattice.lteq(b, c)) || IntAsUIJDLattice.lteq(a, c),
      s"leq should be transitive, but $a leq $b and $b leq $c and $a is not leq $c"
    )
  }

  "merge" in forAll { (a: Int, b: Int, c: Int) =>
    val mergeAB    = IntAsUIJDLattice.merge(a, b)
    val mergeTwice = IntAsUIJDLattice.merge(mergeAB, b)

    assert(
      mergeTwice == mergeAB,
      s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
    )

    val mergeBA = IntAsUIJDLattice.merge(b, a)

    assert(
      mergeBA == mergeAB,
      s"merge should be commutative, but $mergeBA does not equal $mergeAB"
    )

    val mergeAB_C = IntAsUIJDLattice.merge(mergeAB, c)
    val mergeBC   = IntAsUIJDLattice.merge(b, c)
    val mergeA_BC = IntAsUIJDLattice.merge(a, mergeBC)

    assert(
      mergeA_BC == mergeAB_C,
      s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
    )
  }

  "decompose" in forAll { (i: Int) =>
    val decomposed = IntAsUIJDLattice.decompose(i)
    val merged     = decomposed.reduceOption(IntAsUIJDLattice.merge)

    merged match {
      case None =>
      case Some(n) =>
        assert(
          n == i,
          s"merging the atoms produced by decompose should yield the original value, but $n does not equal $i"
        )
    }
  }
}

class SetAsDecomposeLatticeTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  "leq" in forAll { (a: Set[Int], b: Set[Int], c: Set[Int]) =>
    assert(
      SetAsUIJDLattice.lteq(a, a),
      s"leq should be reflexive, but $a is not leq $a"
    )

    assert(
      !(SetAsUIJDLattice.lteq(a, b) && SetAsUIJDLattice.lteq(b, c)) || SetAsUIJDLattice.lteq(a, c),
      s"leq should be transitive, but $a leq $b and $b leq $c and $a is not leq $c"
    )
  }

  "merge" in forAll { (a: Set[Int], b: Set[Int], c: Set[Int]) =>
    val mergeAB    = SetAsUIJDLattice.merge(a, b)
    val mergeTwice = SetAsUIJDLattice.merge(mergeAB, b)

    assert(
      mergeTwice == mergeAB,
      s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
    )

    val mergeBA = SetAsUIJDLattice.merge(b, a)

    assert(
      mergeBA == mergeAB,
      s"merge should be commutative, but $mergeBA does not equal $mergeAB"
    )

    val mergeAB_C = SetAsUIJDLattice.merge(mergeAB, c)
    val mergeBC   = SetAsUIJDLattice.merge(b, c)
    val mergeA_BC = SetAsUIJDLattice.merge(a, mergeBC)

    assert(
      mergeA_BC == mergeAB_C,
      s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
    )
  }

  "decompose" in forAll { (s: Set[Int]) =>
    val decomposed = SetAsUIJDLattice.decompose(s)
    val merged     = decomposed.reduceOption(SetAsUIJDLattice[Int].merge)

    merged match {
      case None =>
      case Some(x) =>
        assert(
          x == s,
          s"merging the atoms produced by decompose should yield the original value, but $x does not equal $s"
        )
    }
  }
}

class OptionAsDecomposeLatticeTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  "leq" in forAll { (a: Option[Int], b: Option[Int], c: Option[Int]) =>
    assert(
      OptionAsUIJDLattice[Int].lteq(a, a),
      s"leq should be reflexive, but $a is not leq $a"
    )

    assert(
      !(OptionAsUIJDLattice[Int].lteq(a, b) && OptionAsUIJDLattice[Int].lteq(b, c)) || OptionAsUIJDLattice[Int].lteq(a, c),
      s"leq should be transitive, but $a leq $b and $b leq $c and $a is not leq $c"
    )
  }

  "merge" in forAll { (a: Option[Int], b: Option[Int], c: Option[Int]) =>
    val mergeAB    = OptionAsUIJDLattice[Int].merge(a, b)
    val mergeTwice = OptionAsUIJDLattice[Int].merge(mergeAB, b)

    assert(
      mergeTwice == mergeAB,
      s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
    )

    val mergeBA = OptionAsUIJDLattice[Int].merge(b, a)

    assert(
      mergeBA == mergeAB,
      s"merge should be commutative, but $mergeBA does not equal $mergeAB"
    )

    val mergeAB_C = OptionAsUIJDLattice[Int].merge(mergeAB, c)
    val mergeBC   = OptionAsUIJDLattice[Int].merge(b, c)
    val mergeA_BC = OptionAsUIJDLattice[Int].merge(a, mergeBC)

    assert(
      mergeA_BC == mergeAB_C,
      s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
    )
  }

  "decompose" in forAll { (o: Option[Int]) =>
    val decomposed = OptionAsUIJDLattice[Int].decompose(o)
    val merged     = decomposed.reduceOption(OptionAsUIJDLattice[Int].merge)

    merged match {
      case None =>
      case Some(x) =>
        assert(
          x == o,
          s"merging the atoms produced by decompose should yield the original value, but $x does not equal $o"
        )
    }
  }
}

class MapAsDecomposeLatticeTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  "leq" in forAll { (a: Map[Int, Int], b: Map[Int, Int], c: Map[Int, Int]) =>
    assert(
      MapAsUIJDLattice[Int, Int].lteq(a, a),
      s"leq should be reflexive, but $a is not leq $a"
    )

    assert(
      !(MapAsUIJDLattice[Int, Int].lteq(a, b) && MapAsUIJDLattice[Int, Int].lteq(b, c)) || MapAsUIJDLattice[Int, Int].lteq(
        a,
        c
      ),
      s"leq should be transitive, but $a leq $b and $b leq $c and $a is not leq $c"
    )
  }

  "merge" in forAll { (a: Map[Int, Int], b: Map[Int, Int], c: Map[Int, Int]) =>
    val mergeAB    = MapAsUIJDLattice[Int, Int].merge(a, b)
    val mergeTwice = MapAsUIJDLattice[Int, Int].merge(mergeAB, b)

    assert(
      mergeTwice == mergeAB,
      s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
    )

    val mergeBA = MapAsUIJDLattice[Int, Int].merge(b, a)

    assert(
      mergeBA == mergeAB,
      s"merge should be commutative, but $mergeBA does not equal $mergeAB"
    )

    val mergeAB_C = MapAsUIJDLattice[Int, Int].merge(mergeAB, c)
    val mergeBC   = MapAsUIJDLattice[Int, Int].merge(b, c)
    val mergeA_BC = MapAsUIJDLattice[Int, Int].merge(a, mergeBC)

    assert(
      mergeA_BC == mergeAB_C,
      s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
    )
  }

  "decompose" in forAll { (m: Map[Int, Int]) =>
    val decomposed = MapAsUIJDLattice[Int, Int].decompose(m)
    val merged     = decomposed.reduceOption(MapAsUIJDLattice[Int, Int].merge)

    merged match {
      case None =>
      case Some(x) =>
        assert(
          x == m,
          s"merging the atoms produced by decompose should yield the original value, but $x does not equal $m"
        )
    }
  }
}

class PairAsDecomposeLatticeTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  "leq" in forAll { (a: (Set[Int], Set[Int]), b: (Set[Int], Set[Int]), c: (Set[Int], Set[Int])) =>
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

  "merge" in forAll { (a: (Set[Int], Set[Int]), b: (Set[Int], Set[Int]), c: (Set[Int], Set[Int])) =>
    val mergeAB    = PairAsUIJDLattice[Set[Int], Set[Int]].merge(a, b)
    val mergeTwice = PairAsUIJDLattice[Set[Int], Set[Int]].merge(mergeAB, b)

    assert(
      mergeTwice == mergeAB,
      s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
    )

    val mergeBA = PairAsUIJDLattice[Set[Int], Set[Int]].merge(b, a)

    assert(
      mergeBA == mergeAB,
      s"merge should be commutative, but $mergeBA does not equal $mergeAB"
    )

    val mergeAB_C = PairAsUIJDLattice[Set[Int], Set[Int]].merge(mergeAB, c)
    val mergeBC   = PairAsUIJDLattice[Set[Int], Set[Int]].merge(b, c)
    val mergeA_BC = PairAsUIJDLattice[Set[Int], Set[Int]].merge(a, mergeBC)

    assert(
      mergeA_BC == mergeAB_C,
      s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
    )
  }

  "decompose" in forAll { (p: (Set[Int], Set[Int])) =>
    val decomposed = PairAsUIJDLattice[Set[Int], Set[Int]].decompose(p)
    val merged     = decomposed.reduceOption(PairAsUIJDLattice[Set[Int], Set[Int]].merge)

    merged match {
      case None =>
      case Some(x) =>
        assert(
          x == p,
          s"merging the atoms produced by decompose should yield the original value, but $x does not equal $p"
        )
    }
  }
}

object LexPairGenerators {
  def genLexPair[A, B](implicit arbA: Arbitrary[A], arbB: Arbitrary[B]): Gen[LexPair[A, B]] =
    for {
      a <- arbA.arbitrary
      b <- arbB.arbitrary
    } yield LexPair(a, b)

  implicit def arbLexPair[A, B](implicit
      arbA: Arbitrary[A],
      arbB: Arbitrary[B]
  ): Arbitrary[LexPair[A, B]] =
    Arbitrary(genLexPair)
}

class LexPairAsDecomposeLatticeTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import LexPairGenerators.*

  "leq" in forAll { (a: LexPair[Set[Int], Set[Int]], b: LexPair[Set[Int], Set[Int]], c: LexPair[Set[Int], Set[Int]]) =>
    assert(
      LexPairAsUIJDLattice[Set[Int], Set[Int]].lteq(a, a),
      s"leq should be reflexive, but $a is not leq $a"
    )

    assert(
      !(LexPairAsUIJDLattice[Set[Int], Set[Int]].lteq(a, b) && LexPairAsUIJDLattice[Set[Int], Set[Int]].lteq(
        b,
        c
      )) || LexPairAsUIJDLattice[Set[Int], Set[Int]].lteq(a, c),
      s"leq should be transitive, but $a leq $b and $b leq $c and $a is not leq $c"
    )
  }

  "merge" in forAll {
    (a: LexPair[Set[Int], Set[Int]], b: LexPair[Set[Int], Set[Int]], c: LexPair[Set[Int], Set[Int]]) =>
      val mergeAB    = LexPairAsUIJDLattice[Set[Int], Set[Int]].merge(a, b)
      val mergeTwice = LexPairAsUIJDLattice[Set[Int], Set[Int]].merge(mergeAB, b)

      assert(
        mergeTwice == mergeAB,
        s"merge should be idempotent, but $mergeTwice does not equal $mergeAB"
      )

      val mergeBA = LexPairAsUIJDLattice[Set[Int], Set[Int]].merge(b, a)

      assert(
        mergeBA == mergeAB,
        s"merge should be commutative, but $mergeBA does not equal $mergeAB"
      )

      val mergeAB_C = LexPairAsUIJDLattice[Set[Int], Set[Int]].merge(mergeAB, c)
      val mergeBC   = LexPairAsUIJDLattice[Set[Int], Set[Int]].merge(b, c)
      val mergeA_BC = LexPairAsUIJDLattice[Set[Int], Set[Int]].merge(a, mergeBC)

      assert(
        mergeA_BC == mergeAB_C,
        s"merge should be associative, but $mergeA_BC does not equal $mergeAB_C"
      )
  }

  "decompose" in forAll { (m: LexPair[Set[Int], Set[Int]]) =>
    val decomposed = LexPairAsUIJDLattice[Set[Int], Set[Int]].decompose(m)
    val merged     = decomposed.reduceOption(LexPairAsUIJDLattice[Set[Int], Set[Int]].merge)

    merged match {
      case None =>
      case Some(x) =>
        assert(
          x == m,
          s"merging the atoms produced by decompose should yield the original value, but $x does not equal $m"
        )
    }
  }
}
