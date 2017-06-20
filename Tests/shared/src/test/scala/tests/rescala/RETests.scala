package tests.rescala

import org.scalactic.source
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import rescala.core.{Engine, Struct}


abstract class RETests extends FunSuite with TableDrivenPropertyChecks {

  type TestStruct  <: Struct

  def engines(es: Engine[_ <: Struct]*)(text: String)(testCase: Engine[TestStruct] => Any)(implicit pos: source.Position): Unit = {
    test(text) {
      forAll(Table("engine", es:_*))(e => testCase(e.asInstanceOf[Engine[TestStruct]]))
    }
  }

  def allEngines(text: String)(testCase: Engine[TestStruct] => Any)(implicit pos: source.Position): Unit = {
    engines(rescala.testhelper.TestEngines.all: _*)(text)(testCase)(pos)
  }
}
