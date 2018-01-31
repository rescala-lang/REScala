package tests.rescala.testtools

import org.scalactic.source
import org.scalatest.FreeSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import rescala.core.{Scheduler, Struct}


abstract class RETests extends FreeSpec with TableDrivenPropertyChecks {

  type TestStruct <: Struct

  def engines(es: Scheduler[_ <: Struct]*)(text: String)(testCase: Scheduler[TestStruct] => Any)(implicit pos: source.Position): Unit = {
    forAll(Table("engine", es: _*)) { e =>
      s"Testing $e" - {text in testCase(e.asInstanceOf[Scheduler[TestStruct]])}
    }
  }

  def allEngines(text: String)(testCase: Scheduler[TestStruct] => Any)(implicit pos: source.Position): Unit = {
    engines(tests.rescala.testtools.TestEngines.all: _*)(text)(testCase)(pos)
  }


  def multiEngined(block: Scheduler[TestStruct] => Any): Unit = {
    for(engine <- tests.rescala.testtools.TestEngines.all) yield {
      s"Testing $engine" - block(engine.asInstanceOf[Scheduler[TestStruct]])
    }
  }

  def test(text: String)(f: => Any): Unit = {
    text in f
  }

}
