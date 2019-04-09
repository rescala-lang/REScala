import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.deltacrdts.dotstores.Dot
import rescala.deltacrdts.dotstores.DotStore._

object DotSetGenerator {
  implicit val genDot: Arbitrary[Dot] = Arbitrary(for {
    id <- Gen.oneOf('a' to 'g')
    value <- Gen.oneOf(0 to 100)
  } yield Dot(id.toString, value))


  implicit val genSet: Arbitrary[Set[Dot]] = Arbitrary(for {
    ids <- Gen.listOf(Gen.oneOf('a' to 'g').map(_.toString)).map(_.toSet)
    if ids.nonEmpty
    value <- Gen.listOfN(ids.size, Gen.oneOf(0 to 100))
    dots <- ids.zip(value).map(e => Dot(e._1,e._2))
  } yield dots)
}


class DotSetTests extends FreeSpec with ScalaCheckDrivenPropertyChecks {
//  import DotSetGenerator._

  "Manual Tests" in {
    val d1 = Dot("1", 1)
    val s = Set(d1)
    assert(s.dots.contains(d1))

//    val t = Set(Dot("2", 1))
//    val union = s.merge(t)
//    val union2 = DotStore.merge(s,t)
//    assert(union.contains(d1) && union.contains(d2))
//    assert(union2.contains(d1) && union2.contains(d2))
  }

//  "merge" in forAll { (s: Set[Dot], t: Set[Dot]) =>
//    // check merge methods
//    val m1 = s.merge(t)
//    val m2 = t.merge(s)
//    val m3 = DotStore.merge(s,t)
//    assert(m1 == m2 && m2 == m3)
//
//    // check if all elements in
//    for (e <- s) yield {
//      assert(m1.contains(e) && m2.contains(e) && m3.contains(e))
//    }
//    for (e <- t) yield {
//      assert(m1.contains(e) && m2.contains(e) && m3.contains(e))
//    }
//
//    // count elements
//    assert(m1.size == m2.size && m2.size == m3.size)
//    assert(m1.size == s.size + t.size - s.intersect(t).size)
//  }

}
