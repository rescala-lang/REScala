package tests.rescala.testtools

import org.scalactic.source
import org.scalatest.FreeSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import rescala.core.Struct
import rescala.interface.RescalaInterface


abstract class RETests extends FreeSpec with TableDrivenPropertyChecks {

  type TestStruct <: Struct

  def engines(es: RescalaInterface[_ <: Struct]*)
             (text: String)
             (testCase: RescalaInterface[TestStruct] => Any)
             (implicit pos: source.Position): Unit = {
    forAll(Table("engine", es: _*)) { e =>
      s"Testing $e" - {text in testCase(e.asInstanceOf[RescalaInterface[TestStruct]])}
    }
  }

  def allEngines(text: String)(testCase: RescalaInterface[TestStruct] => Any)(implicit pos: source.Position): Unit = {
    engines(tests.rescala.testtools.TestEngines.all: _*)(text)(testCase)(pos)
  }


  def multiEngined(block: RescalaInterface[TestStruct] => Any): Unit = {
    for (engine <- tests.rescala.testtools.TestEngines.all) yield {
      s"Testing $engine" - block(engine.asInstanceOf[RescalaInterface[TestStruct]])
    }
  }

  def test(text: String)(f: => Any): Unit = {
    text in f
  }

}
