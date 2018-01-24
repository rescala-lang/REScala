package tests.rescala.reevaluations

import org.scalatest.prop.Whenever
import rescala.infiltration.Infiltrator.assertLevel
import tests.rescala.util.RETests


class ReevaluationCount extends RETests with Whenever {



  allEngines("keep fixed Dependencies"){ engine => import engine._

    val v1 = Var(true)
    val v2 = Var(0)
    val v3 = Var(10)
    var i = 0
    var test = 0

    val s = Signal  {
      i += 1
      if (v1()) v2() else v3()
    }

    s.changed += (_ => test += 1)

    assert(test == 0)
    v2.set(1)
    assert(test == 1)

    v1.set(false)
    assert(test == 2)
    v3.set(11)
    assert(test == 3)

    v2.set(2)
    assert(test == 3)

    v1.set(true)
    assert(test == 4)
    v2.set(3)
    assert(test == 5)

  }


}
