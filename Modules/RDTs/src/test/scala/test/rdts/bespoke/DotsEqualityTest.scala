package test.rdts.bespoke

import rdts.base.Uid
import rdts.time.{Dot, Dots}
import test.rdts.given

class DotsEqualityTest extends munit.FunSuite {

  test("basic usage") {
    val uid1 = Uid.predefined("abc")
    val uid2 = Uid.predefined("def")

    val dots = List(
      Dots.from(List(Dot(uid1, 123), Dot(uid1, 1234),  Dot(uid2, 12345), Dot(uid2, 123456))),
      Dots.from(List(Dot(uid1, 123), Dot(uid1, 12345), Dot(uid2, 123), Dot(uid2, 1234)))
    )

    val head = dots.headOption.getOrElse(Dots.empty)

    assert(!dots.map[Boolean](d => d == head).reduce((b1, b2) => b1 & b2))
    assert(!dots.drop(1).forall(d => d == head))
    assertEquals(Set.from(dots).size, 2)
    assertEquals(dots.distinct.size, 2)

  }

}
