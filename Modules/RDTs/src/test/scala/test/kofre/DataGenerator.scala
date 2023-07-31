package test.kofre

import kofre.base.*
import kofre.datatypes.*
import kofre.datatypes.GrowOnlyList.Node
import kofre.datatypes.alternatives.{MultiValueRegister, ObserveRemoveSet}
import kofre.datatypes.contextual.*
import kofre.dotted.*
import kofre.time.*
import org.scalacheck.{Arbitrary, Gen, Shrink}

import scala.annotation.nowarn

object DataGenerator {

  given arbId: Arbitrary[Uid] = Arbitrary(Gen.oneOf('a' to 'g').map(c => Uid.predefined(c.toString)))

  given arbVectorClock: Arbitrary[VectorClock] = Arbitrary:
    for
      ids: Set[Uid]     <- Gen.nonEmptyListOf(arbId.arbitrary).map(_.toSet)
      value: List[Long] <- Gen.listOfN(ids.size, Gen.oneOf(0L to 100L))
    yield VectorClock.fromMap(ids.zip(value).toMap)

  val smallNum = Gen.chooseNum(-10, 10)

  given arbCausalTime: Arbitrary[CausalTime] = Arbitrary:
    for {
      time     <- smallNum
      causal   <- smallNum
      nanotime <- Gen.long
    } yield CausalTime(time, causal, nanotime)

  given arbLww[E: Arbitrary]: Arbitrary[LastWriterWins[E]] = Arbitrary:
    for
      causal <- arbCausalTime.arbitrary
      value  <- Arbitrary.arbitrary
    yield LastWriterWins(causal, value)

  given arbGcounter: Arbitrary[GrowOnlyCounter] = Arbitrary(
    Gen.mapOf[Uid, Int](Gen.zip(arbId.arbitrary, smallNum)).map(GrowOnlyCounter(_))
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
      version <- arbVectorClock.arbitrary
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

  val genDot: Gen[Dot] = for {
    id    <- Gen.oneOf('a' to 'g')
    value <- Gen.oneOf(0 to 100)
  } yield Dot(Uid.predefined(id.toString), value)

  val uniqueDot: Gen[Dot] = for {
    id    <- Gen.oneOf('a' to 'g')
    value <- Gen.long
  } yield Dot(Uid.predefined(id.toString), value)

  given arbDot: Arbitrary[Dot] = Arbitrary(genDot)

  given arbDots: Arbitrary[Dots] = Arbitrary:
    Gen.containerOf[Set, Dot](genDot).map(Dots.from)

  given Arbitrary[ArrayRanges] = Arbitrary(
    for
      x <- Gen.listOf(smallNum)
    yield ArrayRanges.from(x.map(_.toLong))
  )

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

  given arbDotSet: Arbitrary[DotSet] = Arbitrary:
    arbDots.arbitrary.map(DotSet.apply)

  case class SmallTimeSet(s: Set[Time])

  given Arbitrary[SmallTimeSet] = Arbitrary(for {
    contents <- Gen.listOf(Gen.chooseNum(0L, 100L))
  } yield (SmallTimeSet(contents.toSet)))

  given arbGrowOnlyList[E](using arb: Arbitrary[E]): Arbitrary[GrowOnlyList[E]] = Arbitrary:
    Gen.listOf(arb.arbitrary).map: list =>
      GrowOnlyList.empty.insertAllGL(0, list)

  def badInternalGrowOnlyList[E](using arb: Arbitrary[E]): Arbitrary[GrowOnlyList[E]] = Arbitrary:
    Gen.listOf(arbLww(using arb).arbitrary).map: list =>
      val elems: List[Node.Elem[LastWriterWins[E]]] = list.map(GrowOnlyList.Node.Elem.apply)
      val pairs = elems.distinct.sortBy(_.value.timestamp).sliding(2).flatMap:
        case Seq(l, r) => Some((l) -> (r))
        case _         => None // filters out uneven numbers of elements
      val all =
        elems.headOption.map(GrowOnlyList.Node.Head -> _) concat pairs
      GrowOnlyList(all.toMap)

  given arbTwoPhaseSet[E](using arb: Arbitrary[E]): Arbitrary[TwoPhaseSet[E]] = Arbitrary:
    Gen.listOf(arb.arbitrary).flatMap: additions =>
      Gen.listOf(arb.arbitrary).map: removals =>
        TwoPhaseSet(additions.toSet, removals.toSet)

  given arbDotted[E: HasDots](using arb: Arbitrary[E]): Arbitrary[Dotted[E]] = Arbitrary:
    for
      dots <- Arbitrary.arbitrary[Dots]
      elem <- arb.arbitrary
    yield Dotted(elem, dots union elem.dots)

  given arbDotmap[K, V: HasDots](using arbElem: Arbitrary[K], arbKey: Arbitrary[V]): Arbitrary[DotMap[K, V]] =
    Arbitrary:
      Gen.listOf(Gen.zip[K, V](arbElem.arbitrary, arbKey.arbitrary)).map: pairs =>
        // remove dots happens to normalize the structure to remove empty inner elements
        DotMap(pairs.toMap).removeDots(Dots.empty).getOrElse(DotMap(Map.empty))

  @nowarn
  given shrinkDotted[A: HasDots]: Shrink[Dotted[A]] = Shrink: dotted =>
    (dotted.context.decomposed.iterator concat dotted.context.iterator.map(Dots.single)).toStream.flatMap: e =>
      dotted.data.removeDots(e).map(Dotted(_, dotted.context.subtract(e)))

  given arbCMultiVersion[E](using arb: Arbitrary[E]): Arbitrary[contextual.MultiVersionRegister[E]] = Arbitrary:
    Gen.listOf(Gen.zip(uniqueDot, arb.arbitrary)).map: pairs =>
      MultiVersionRegister(DotFun(pairs.toMap))

  given arbEnableWinsFlag: Arbitrary[contextual.EnableWinsFlag] = Arbitrary:
    arbDotSet.arbitrary.map(EnableWinsFlag.apply)

}
