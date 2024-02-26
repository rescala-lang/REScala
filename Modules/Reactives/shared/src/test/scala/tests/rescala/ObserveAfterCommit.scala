package tests.rescala.misc


import reactives.default._

class ObserveAfterCommit extends munit.FunSuite {

  test("observe after TX throws") {

    val v1 = Var(1)

    val next = v1.map { i => () => v1.set(i + 1) }

    val callback = next.now

    intercept[IllegalStateException] { callback() }

  }

}
