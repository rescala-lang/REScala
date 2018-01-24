package tests.rescala

import tests.rescala.util.RETests

class GlitchFreedomTestSuite extends RETests {

  allEngines("no Glitches In Simple Case") { engine =>
    import engine._

    val v1 = Var(1)
    val s1 = v1.map {2 * _}
    val s2 = v1.map {3 * _}
    val s3 = Signals.lift(s1, s2) {_ + _}

    val s1List = s1.changed.list()
    val s2List = s2.changed.list()
    val s3List = s3.changed.list()

    v1.set(3)

    assert(s1List.now === List(6))
    assert(s2List.now === List(9))
    assert(s3List.now === List(15))

  }

}
