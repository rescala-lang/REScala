package tests.rescala.testtools

import org.scalactic.source
import org.scalatest.Tag
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import reactives.operator.Interface

abstract class RETests extends AnyFreeSpec with TableDrivenPropertyChecks {

  def engines(es: Interface*)(
      text: String,
      tags: List[Tag] = Nil
  )(testCase: Interface => Any)(implicit pos: source.Position): Unit = {
    forAll(Table("engine", es*)) { e =>
      val testEngine = e
      s"Testing $testEngine" - (tags match {
        case Nil          => text in testCase(testEngine)
        case head :: tail => text.taggedAs(head, tail*) in testCase(testEngine)
      })
    }
  }

  def allEngines(text: String)(testCase: Interface => Any)(implicit pos: source.Position): Unit = {
    engines(tests.rescala.testtools.TestEngines.all*)(text)(testCase)(pos)
  }

  def multiEngined(block: Interface => Any): Unit = {
    for (engine <- tests.rescala.testtools.TestEngines.all) {
      s"Testing $engine" - { block(engine); () }
    }
  }

  def test(text: String)(f: => Any): Unit = {
    text in f
  }

}
