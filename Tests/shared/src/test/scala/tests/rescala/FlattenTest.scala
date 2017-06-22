package tests.rescala


class FlattenTest extends RETests {

    allEngines("flatten var") { engine => import engine._
      val sv = Signal { Var(10) }.flatten
      val vv = Var(Var(10)).flatten
      val vs = Var(Signal{10}).flatten
      val ss = Signal(Signal(10)).flatten
      assert (sv.now === 10)
      assert (vv.now === 10)
      assert (vs.now === 10)
      assert (ss.now === 10)
    }

  allEngines("flatten array") { engine => import engine._
    val sv = Signal { Array(Var(10)) }.flatten
    assert (sv.now == Array(10))

  }
}
