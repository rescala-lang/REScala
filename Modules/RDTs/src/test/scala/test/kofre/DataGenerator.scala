package test.kofre

import org.scalacheck.{Arbitrary, Gen}
import kofre.dotted.*
import kofre.time.*
import kofre.base.*
import kofre.datatypes.*
import kofre.datatypes.alternatives.lww.GenericLastWriterWins
import kofre.datatypes.alternatives.{MultiValueRegister, ObserveRemoveSet}

object DataGenerator {

  given arbId: Arbitrary[Uid] = Arbitrary(Gen.oneOf('a' to 'g').map(_.toString))

  given arbVersion: Arbitrary[VectorClock] = Arbitrary(for {
    ids: Set[Uid]     <- Gen.nonEmptyListOf(arbId.arbitrary).map(_.toSet)
    value: List[Long] <- Gen.listOfN(ids.size, Gen.oneOf(0L to 100L))
  } yield VectorClock.fromMap(ids.zip(value).toMap))

  given arbGenericLww: Arbitrary[GenericLastWriterWins[Time, Int]] = Arbitrary(
    for {
      time  <- Gen.long
      value <- Gen.choose(Int.MinValue, Int.MaxValue)
    } yield GenericLastWriterWins(time, value)
  )

  given arbLww: Arbitrary[Dotted[LastWriterWins[Int]]] = Arbitrary(
    for {
      time  <- Gen.long
      dot   <- arbDot.arbitrary
      value <- Gen.choose(Int.MinValue, Int.MaxValue)
    } yield Dotted(LastWriterWins(dot, time, value), Dots.single(dot))
  )

  given arbOptLww: Arbitrary[Dotted[Option[LastWriterWins[Int]]]] = Arbitrary(
    for {
      lww <- arbLww.arbitrary
    } yield lww.map(Some.apply)
  )

  given arbTupleOptLww: Arbitrary[Dotted[(Option[LastWriterWins[Int]], Option[LastWriterWins[Int]])]] = Arbitrary(
    for {
      left <- arbLww.arbitrary
      right <- arbLww.arbitrary
    } yield Dotted((Some(left.store), Some(right.store)), left.context union right.context)
  )

  given Lattice[Dotted[(Option[LastWriterWins[Int]], Option[LastWriterWins[Int]])]] = DottedLattice.derived

  given arbGcounter: Arbitrary[GrowOnlyCounter] = Arbitrary(
    Gen.mapOf[Uid, Int](Gen.zip(arbId.arbitrary, Arbitrary.arbitrary[Int])).map(GrowOnlyCounter(_))
  )

  given arbPosNeg: Arbitrary[PosNegCounter] = Arbitrary(
    for {
      pos <- arbGcounter.arbitrary
      neg <- arbGcounter.arbitrary
    } yield (PosNegCounter(pos, neg))
  )

  given Lattice[Int] = _ max _

  given arbORSet[A: Arbitrary]: Arbitrary[ObserveRemoveSet[A]] = Arbitrary(for {
    added   <- Gen.nonEmptyListOf(Arbitrary.arbitrary[A])
    removed <- Gen.listOf(Gen.oneOf(added))
  } yield {
    val a = added.foldLeft(ObserveRemoveSet.empty[A])((s, v) => Lattice.merge(s, s.add(v)))
    removed.foldLeft(a)((s, v) => Lattice.merge(s, s.remove(v)))
  })

  given arbMVR[A: Arbitrary]: Arbitrary[MultiValueRegister[A]] =
    val pairgen = for {
      version <- arbVersion.arbitrary
      value   <- Arbitrary.arbitrary[A]
    } yield (version, value)
    val map = Gen.listOf(pairgen).map(vs => MultiValueRegister(vs.toMap))
    Arbitrary(map)

  given arbCausalQueue[A: Arbitrary]: Arbitrary[Dotted[CausalQueue[A]]] =
    val pairgen = for {
      id    <- arbId.arbitrary
      value <- Arbitrary.arbitrary[A]
    } yield (id, value)
    val map = Gen.listOf(pairgen).map(_.foldLeft(Dotted(CausalQueue.empty[A])) { case (acc, (id, value)) =>
      acc merge acc.causalQueue.enqueue(using id)(value)
    })
    Arbitrary(map)

  implicit val genDot: Gen[Dot] = for {
    id    <- Gen.oneOf('a' to 'g')
    value <- Gen.oneOf(0 to 100)
  } yield Dot(id.toString, value)

  implicit val arbDot: Arbitrary[Dot] = Arbitrary(genDot)

  val genDotSet: Gen[Set[Dot]] = Gen.containerOf[Set, Dot](genDot)

  val genDietMapCContext: Gen[Dots] = for {
    ds <- genDotSet
  } yield Dots.from(ds)

  implicit val arbDietMapCContext: Arbitrary[Dots] = Arbitrary(genDietMapCContext)

  implicit val arbDotSet: Arbitrary[Set[Dot]] = Arbitrary(genDotSet)

  def genDotFun[A](implicit g: Arbitrary[A]): Gen[DotFun[A]] = for {
    n      <- Gen.posNum[Int]
    dots   <- Gen.containerOfN[List, Dot](n, genDot)
    values <- Gen.containerOfN[List, A](n, g.arbitrary)
  } yield DotFun((dots zip values).toMap)

  implicit def arbDotFun[A](implicit g: Arbitrary[A]): Arbitrary[DotFun[A]] = Arbitrary(genDotFun)

  def makeUnique(rem: List[Dots], acc: List[Dots], state: Dots): List[Dots] =
    rem match
      case Nil    => acc
      case h :: t => makeUnique(t, h.subtract(state) :: acc, state union h)

  def genDotMap[K](implicit gk: Arbitrary[K]): Gen[Map[K, Dots]] =
    (for {
      n         <- Gen.posNum[Int]
      keys      <- Gen.listOfN(n, gk.arbitrary)
      dupvalues <- Gen.listOfN(n, arbDietMapCContext.arbitrary)
    } yield {
      (keys zip makeUnique(dupvalues, Nil, Dots.empty)).toMap
    })

  implicit val arbrealDotSet: Arbitrary[DotSet] = Arbitrary(genDietMapCContext.map(DotSet.apply))

  implicit def arbDotMap[K](implicit gk: Arbitrary[K]): Arbitrary[Map[K, DotSet]] =
    Arbitrary(genDotMap[K].map { _.map((k, v) => k -> DotSet(v)) })

  implicit def arbRealDotmap[K](implicit gk: Arbitrary[K]): Arbitrary[DotMap[K, DotSet]] =
    Arbitrary(arbDotMap.arbitrary.map(DotMap.apply))

  case class SmallTimeSet(s: Set[Time])

  given Arbitrary[SmallTimeSet] = Arbitrary(for {
    contents <- Gen.listOf(Gen.chooseNum(0L, 100L))
  } yield (SmallTimeSet(contents.toSet)))
}
