package tests.rescala


import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn
import rescala.reactives.Signals

import scala.language.implicitConversions




class GlitchFreedomTestSuite extends RETests {




  allEngines("noGlitchesInSimpleCase"){ engine => import engine._

    val v1 = Var(1)
    val s1 = v1.map { 2 * _ }
    val s2 = v1.map { 3 * _ }
    val s3 = Signals.static(s1, s2) { t => s1.get(t) + s2.get(t) }

    val s1List = s1.changed.list()
    val s2List = s2.changed.list()
    val s3List = s3.changed.list()

    v1.set(3)

    assert(s1List.now === List(6))
    assert(s2List.now === List(9))
    assert(s3List.now === List(15))

  }

}
