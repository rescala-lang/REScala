package tests.rescala

import tests.rescala.util.RETests

class DisconnectTests extends RETests {

  allEngines("remove incoming dependencies when disconnecting signals") { engine => import engine._
    val v1 = Var(10)
    val m1 = v1.map(_ + 10)
    val m2 = m1.map(_ + 10)


    assert(v1.now === 10)
    assert(m1.now === 20)
    assert(m2.now === 30)

    v1.set(12)
    assert(v1.now === 12)
    assert(m1.now === 22)
    assert(m2.now === 32)

    m1.disconnect()

    v1.set(16)
    assert(v1.now === 16)
    assert(m1.now === 22)
    assert(m2.now === 32)

  }

}
