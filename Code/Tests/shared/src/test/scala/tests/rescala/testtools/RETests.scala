package tests.rescala.testtools

import org.scalactic.source
import org.scalatest.Tag
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import rescala.core.Struct
import rescala.interface.RescalaInterface

abstract class RETests extends AnyFreeSpec with TableDrivenPropertyChecks {

  type TestStruct <: Struct

  def engines(es: RescalaInterface[_ <: Struct]*)(
      text: String,
      tags: List[Tag] = Nil
  )(testCase: RescalaInterface[TestStruct] => Any)(implicit pos: source.Position): Unit = {
    forAll(Table("engine", es: _*)) { e =>
      val testEngine = e.asInstanceOf[RescalaInterface[TestStruct]]
      s"Testing $testEngine" - (tags match {
        case Nil          => text in testCase(testEngine)
        case head :: tail => text taggedAs (head, tail: _*) in testCase(testEngine)
      })
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
