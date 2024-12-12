package ex2024travel.lofi_acl.example.travelplanner

import munit.FunSuite

class TravelPlanTest extends FunSuite {
  test("changeTitle") {
    var uut = TravelPlan.empty
    uut = uut.merge(uut.changeTitle("Test"))
    assertEquals(uut.title.read, "Test")
    uut = uut.merge(uut.changeTitle("Test 2"))
    assertEquals(uut.title.read, "Test 2")

    val delta1 = uut.changeTitle("A")
    val delta2 = uut.changeTitle("B")

    assertEquals(uut.merge(delta1).merge(delta2).title.read, "B")
    assertEquals(uut.merge(delta2).merge(delta1).title.read, "B")
  }
}
