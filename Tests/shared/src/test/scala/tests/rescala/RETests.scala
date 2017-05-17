package tests.rescala

import org.scalactic.source
import org.scalatest.FunSuite
import org.scalatest.prop.{TableDrivenPropertyChecks}
import rescala.Engines
import rescala.engine.{Engine, Turn}
import rescala.graph.Struct
import rescala.levelbased.LevelStruct


abstract class RETests extends FunSuite with TableDrivenPropertyChecks {

  type S  <: Struct
  type LS <: LevelStruct

  def engines(es: Engines.TEngine*)(text: String)(testCase: Engine[S, Turn[S]] => Any)(implicit pos: source.Position): Unit = {
    test(text) {
      forAll(Table("engine", es:_*))(e => testCase(e.asInstanceOf[Engine[S, Turn[S]]]))
    }
  }

  def allEngines(text: String)(testCase: Engine[S, Turn[S]] => Any)(implicit pos: source.Position): Unit = {
    engines(rescala.testhelper.TestEngines.all: _*)(text)(testCase)(pos)
  }
}
