package tests.rescala

import java.{lang => jl, util => ju}

import org.junit.runners.Parameterized.Parameters
import rescala.graph.State
import rescala.turns.{Engine, Engines, Turn}

// thanks to: http://stackoverflow.com/questions/4399881/parameterized-unit-tests-in-scala-with-junit4
class JUnitParameters {
  @Parameters def parameters: ju.Collection[Array[Engines.NoLockType]] = {
    val list = new ju.ArrayList[Array[Engines.NoLockType]]()
    Engines.all.foreach(e => list.add(Array(e)))
    list
  }
}
