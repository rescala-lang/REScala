package tests

import _root_.rescala.meta.MetaBundle

class MetabundleTests extends org.scalatest.freespec.AnyFreeSpec {

  val mb = new MetaBundle {}
  import mb.*

  "basic" in {

    val source = Signal(5)
    val derived = Signal {
      source.value + 1
    }

    assert(derived == Signal(Seq(source), derived.name))

  }

}
