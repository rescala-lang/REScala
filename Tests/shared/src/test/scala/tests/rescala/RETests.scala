package tests.rescala

import org.scalactic.source
import org.scalatest.FunSuite
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1}
import rescala.Engines
import rescala.engine.{Engine, Turn}
import rescala.graph.Struct
import rescala.levelbased.LevelStruct

import scala.language.existentials


abstract class RETests extends FunSuite with TableDrivenPropertyChecks {

  type S  <: Struct
  type LS <: LevelStruct

  private val engines: TableFor1[Engines.TEngine] = Table("engine", rescala.testhelper.TestEngines.all: _*)

  def allEngines(text: String)(testCase: Engine[S, Turn[S]] => Unit)(implicit pos: source.Position): Unit = {
    test(text) {
      forAll(engines)(e => testCase(e.asInstanceOf[Engine[S, Turn[S]]]))
    }
  }


}
