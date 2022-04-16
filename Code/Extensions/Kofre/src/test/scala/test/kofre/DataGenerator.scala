package test.kofre

import kofre.primitives.{CausalQueue, LastWriterWins, MultiValueRegister}
import kofre.sets.ORSet
import kofre.{Defs, Lattice}
import org.scalacheck.{Arbitrary, Gen}
import kofre.causality.VectorClock

object DataGenerator {

  given arbId: Arbitrary[Defs.Id] = Arbitrary(Gen.oneOf('a' to 'g').map(_.toString))

  given arbVersion: Arbitrary[VectorClock] = Arbitrary(for {
    ids: Set[Defs.Id] <- Gen.nonEmptyListOf(arbId.arbitrary).map(_.toSet)
    value: List[Long] <- Gen.listOfN(ids.size, Gen.oneOf(0L to 100L))
  } yield VectorClock.fromMap(ids.zip(value).toMap))

  given arbLww: Arbitrary[LastWriterWins[Int]] = Arbitrary(
    for {
      time  <- Gen.long
      value <- Gen.choose(Int.MinValue, Int.MaxValue)
    } yield LastWriterWins(time, value)
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
}
