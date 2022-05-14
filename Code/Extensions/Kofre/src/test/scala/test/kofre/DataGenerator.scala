package test.kofre

import kofre.base.Defs.Time
import kofre.base.{Defs, Lattice}
import kofre.causality.{CausalContext, Dot, VectorClock}
import kofre.contextual.{AsCausalContext, ContextDecompose}
import kofre.dotted.{DotFun, DotMap, DotSet}
import kofre.predef.{GrowOnlyCounter, PosNegCounter}
import kofre.primitives.{CausalQueue, LastWriterWins, MultiValueRegister}
import kofre.sets.ORSet
import org.scalacheck.{Arbitrary, Gen}

object DataGenerator {

  given arbId: Arbitrary[Defs.Id] = Arbitrary(Gen.oneOf('a' to 'g').map(_.toString))

  given arbVersion: Arbitrary[VectorClock] = Arbitrary(for {
    ids: Set[Defs.Id] <- Gen.nonEmptyListOf(arbId.arbitrary).map(_.toSet)
    value: List[Long] <- Gen.listOfN(ids.size, Gen.oneOf(0L to 100L))
  } yield VectorClock.fromMap(ids.zip(value).toMap))

  given arbLww: Arbitrary[LastWriterWins[Defs.Time, Int]] = Arbitrary(
    for {
      time  <- Gen.long
      value <- Gen.choose(Int.MinValue, Int.MaxValue)
    } yield LastWriterWins(time, value)
  )

  given arbGcounter: Arbitrary[GrowOnlyCounter] = Arbitrary(
    Gen.mapOf[Defs.Id, Int](Gen.zip(arbId.arbitrary, Arbitrary.arbitrary[Int])).map(GrowOnlyCounter(_))
  )

  given arbPosNeg: Arbitrary[PosNegCounter] = Arbitrary(
    for {
      pos <- arbGcounter.arbitrary
      neg <- arbGcounter.arbitrary
    } yield (PosNegCounter(pos, neg))
  )

  given Lattice[Int] = _ max _

  given arbORSet[A: Arbitrary]: Arbitrary[ORSet[A]] = Arbitrary(for {
    added   <- Gen.nonEmptyListOf(Arbitrary.arbitrary[A])
    removed <- Gen.listOf(Gen.oneOf(added))
  } yield {
    val a = added.foldLeft(ORSet.empty[A])((s, v) => Lattice.merge(s, s.add(v)))
    removed.foldLeft(a)((s, v) => Lattice.merge(s, s.remove(v)))
  })

  given arbMVR[A: Arbitrary]: Arbitrary[MultiValueRegister[A]] =
    val pairgen = for {
      version <- arbVersion.arbitrary
      value   <- Arbitrary.arbitrary[A]
    } yield (version, value)
    val map = Gen.listOf(pairgen).map(vs => MultiValueRegister(vs.toMap))
    Arbitrary(map)

  given arbCausalQueue[A: Arbitrary]: Arbitrary[CausalQueue[A]] =
    val pairgen = for {
      id    <- arbId.arbitrary
      value <- Arbitrary.arbitrary[A]
    } yield (id, value)
    val map = Gen.listOf(pairgen).map(_.foldLeft(CausalQueue.empty[A]) { case (acc, (id, value)) =>
      acc.enqueue(value, id)
    })
    Arbitrary(map)

  implicit val genDot: Gen[Dot] = for {
    id    <- Gen.oneOf('a' to 'g')
    value <- Gen.oneOf(0 to 100)
  } yield Dot(id.toString, value)

  implicit val arbDot: Arbitrary[Dot] = Arbitrary(genDot)

  val genDotSet: Gen[Set[Dot]] = Gen.containerOf[Set, Dot](genDot)

  val genDietMapCContext: Gen[CausalContext] = for {
    ds <- genDotSet
  } yield CausalContext.fromSet(ds)

  implicit val arbDietMapCContext: Arbitrary[CausalContext] = Arbitrary(genDietMapCContext)

  implicit val arbDotSet: Arbitrary[Set[Dot]] = Arbitrary(genDotSet)

  def genDotFun[A](implicit g: Arbitrary[A]): Gen[DotFun[A]] = for {
    n      <- Gen.posNum[Int]
    dots   <- Gen.containerOfN[List, Dot](n, genDot)
    values <- Gen.containerOfN[List, A](n, g.arbitrary)
  } yield DotFun((dots zip values).toMap)

  implicit def arbDotFun[A](implicit g: Arbitrary[A]): Arbitrary[DotFun[A]] = Arbitrary(genDotFun)

  def genDotMap[K, V: AsCausalContext](implicit gk: Arbitrary[K], gv: Arbitrary[V]): Gen[Map[K, V]] = (for {
    n      <- Gen.posNum[Int]
    keys   <- Gen.containerOfN[List, K](n, gk.arbitrary)
    values <- Gen.containerOfN[List, V](n, gv.arbitrary)
  } yield (keys zip values).toMap).suchThat { m =>
    val dotsIter = m.values.flatMap(v => AsCausalContext[V].dots(v).iterator)
    val dotsSet  = dotsIter.toSet
    dotsIter.size == dotsSet.size
  }

  implicit val arbrealDotSet: Arbitrary[DotSet] = Arbitrary(genDietMapCContext.map(DotSet.apply))

  implicit def arbDotMap[K, V: AsCausalContext](implicit gk: Arbitrary[K], gv: Arbitrary[V]): Arbitrary[Map[K, V]] =
    Arbitrary(genDotMap)

  implicit def arbRealDotmap[K, V: AsCausalContext](implicit gk: Arbitrary[K], gv: Arbitrary[V]): Arbitrary[DotMap[K, V]] =
    Arbitrary(genDotMap[K, V].map(DotMap.apply))

  case class SmallTimeSet(s: Set[Time])

  given Arbitrary[SmallTimeSet] = Arbitrary(for {
    contents <- Gen.listOf(Gen.chooseNum(0L, 100L))
  } yield (SmallTimeSet(contents.toSet)))
}
