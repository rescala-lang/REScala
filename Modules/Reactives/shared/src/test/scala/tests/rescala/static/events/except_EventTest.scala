package tests.rescala.static.events

import reactives.default.*

class except_EventTest extends munit.FunSuite {

  test("handler Of except  Is Executed If Basic Event Fires") {
    var test = 0
    val e1 = Evt[Int]()
    val e2 = Evt[Int]()
    val e1_except_e2 = e1 `except` e2
    e1_except_e2 `observe` ((_: Int) => {test += 1})

    e1.fire(10)
    assertEquals(test, 1)

  }

  test("handler Of except  Ignores The Second Event If Fires") {
    var test = 0
    val e1 = Evt[Int]()
    val e2 = Evt[Int]()
    val e1_except_e2 = e1 `except` e2
    e1_except_e2 `observe` ((_: Int) => {test += 1})

    e2.fire(10)
    assertEquals(test, 0)

  }

  test("handler Of except  Is Executed Only If First Event Fires And Not The Second") {

    var test = 0

    var cond = false
    val e1 = Evt[Int]()
    val e2 = e1 `map` ((x: Int) => x * 2)
    val e3 = e1 `filter` (_ => cond)
    val e2_except_e3 = e2 `except` e3
    e2_except_e3 `observe` ((_: Int) => {test += 1})

    e1.fire(10)
    assertEquals(test, 1)

    cond = true
    e1.fire(10)
    assertEquals(test, 1)

    cond = false
    e1.fire(10)
    assertEquals(test, 2)

  }

  test("handler Of except  Gets The Correct Value") {

    var value = 0

    var cond = false
    val e1 = Evt[Int]()
    val e2 = e1 `map` ((x: Int) => x)
    val e3 = (e1 `map` ((x: Int) => x * 2)) `filter` (_ => cond)
    val e1_except_e2 = e2 `except` e3
    e1_except_e2 `observe` ((x: Int) => {value = x})

    e1.fire(10)
    assertEquals(value, 10)

    cond = true
    e1.fire(11)
    assertEquals(value, 10)

    cond = false
    e1.fire(12)
    assertEquals(value, 12)

  }

}
