package tests.rescala

import java.{lang => jl, util => ju}

import org.junit.runners.Parameterized.Parameters
import rescala.engines.JVMEngines

// thanks to: http://stackoverflow.com/questions/4399881/parameterized-unit-tests-in-scala-with-junit4
class JUnitParameters {
  @Parameters def parameters: ju.Collection[Array[JVMEngines.TEngine]] = {
    val list = new ju.ArrayList[Array[JVMEngines.TEngine]]()
    JVMEngines.all.foreach(e => list.add(Array(e)))
    list
  }
}
