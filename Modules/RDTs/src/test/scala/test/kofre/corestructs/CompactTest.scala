package test.kofre.corestructs

import kofre.base.{Lattice, Uid}
import kofre.dotted.Dotted
import kofre.time.{ArrayRanges, Dot, Dots, Time}
import org.scalacheck.Prop.*
import test.kofre.DataGenerator.*
import kofre.base.Lattice.Operators


class CompactTest extends munit.ScalaCheckSuite {

  def compact[T](rem: List[Dotted[T]], acc: List[Dotted[T]])(using dl: Lattice[T]): List[Dotted[T]] = rem match
    case Nil => acc
    case h :: tail =>
      def overlap(e: Dotted[T]): Boolean = !h.context.disjunct(e.context)

      val (tin, tother) = tail.partition(overlap)
      val (accin, accother) = acc.partition(overlap)
      val all = tin ++ accin
      val compacted = all.foldLeft(h): (l, r) =>
        Dotted(dl.merge(l.data, r.data), l.context union r.context)

      // have to repeat the check with compacted, until it did not grow
      if all.isEmpty
      then compact(tother, compacted :: accother)
      else compact(compacted :: tother, accother)

  test("basic compact test") {
    val a1                           = Uid.gen()

    val left   = Dotted(Set("a"), Dots.empty.add(Dot(a1, 1)))
    val middle = Dotted(Set("c"), Dots.empty.add(Dot(a1, 2)))
    val right  = Dotted(Set("b"), Dots.empty.add(Dot(a1, 1)))

    val res: List[Dotted[Set[_root_.java.lang.String]]] = compact(
      List(
        left,
        middle,
        right,
      ),
      Nil
    )

    assertEquals(res, List(middle, left merge right))

  }

}
