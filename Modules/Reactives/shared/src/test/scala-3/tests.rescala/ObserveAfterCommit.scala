package tests.rescala.misc

import org.scalatest.freespec.AnyFreeSpec

import rescala.default._

class ObserveAfterCommit extends AnyFreeSpec {

  "observe after TX throws" in {

    val v1 = Var(1)

    val next = v1.map { i => () => v1.set(i + 1) }

    val callback = next.now

    assertThrows[IllegalStateException] { callback() }

  }

}
