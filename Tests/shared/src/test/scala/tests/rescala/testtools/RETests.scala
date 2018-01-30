package tests.rescala.testtools

import org.scalactic.source
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import rescala.core.{Scheduler, Struct}


abstract class RETests extends FunSuite with TableDrivenPropertyChecks {

  type TestStruct  <: Struct

  def engines(es: Scheduler[_ <: Struct]*)(text: String)(testCase: Scheduler[TestStruct] => Any)(implicit pos: source.Position): Unit = {
    test(text) {
      forAll(Table("engine", es:_*))(e => testCase(e.asInstanceOf[Scheduler[TestStruct]]))
    }
  }

  def allEngines(text: String)(testCase: Scheduler[TestStruct] => Any)(implicit pos: source.Position): Unit = {
    engines(tests.rescala.testtools.TestEngines.all: _*)(text)(testCase)(pos)
  }
}
