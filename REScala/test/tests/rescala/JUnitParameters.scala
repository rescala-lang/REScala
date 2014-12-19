package tests.rescala

import java.{lang => jl, util => ju}

import org.junit.runners.Parameterized.Parameters
import rescala.turns.{Engine, Engines, Turn}

// thanks to: http://stackoverflow.com/questions/4399881/parameterized-unit-tests-in-scala-with-junit4
class JUnitParameters {
  @Parameters def parameters: ju.Collection[Array[Engine[Turn]]] = {
    val list = new ju.ArrayList[Array[Engine[Turn]]]()
    Engines.all.foreach(e => list.add(Array(e)))
    list
  }
}
