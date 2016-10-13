package tests.rescala

import org.scalactic.source
import org.scalatest.FunSuite
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1}
import rescala.engines.Engine
import rescala.graph.{LevelStruct, Struct}
import rescala.propagation.Turn

import scala.language.existentials


abstract class RETests extends FunSuite with TableDrivenPropertyChecks {

  type S  <: Struct
  type LS <: LevelStruct

  private val engines: TableFor1[rescala.engines.Engines.TEngine] = Table("engine", rescala.engines.Engines.all: _*)

  def allEngines(text: String)(testCase: Engine[S, Turn[S]] => Unit)(implicit pos: source.Position): Unit = {
    test(text) {
      forAll(engines)(e => testCase(e.asInstanceOf[Engine[S, Turn[S]]]))
    }
  }


}
