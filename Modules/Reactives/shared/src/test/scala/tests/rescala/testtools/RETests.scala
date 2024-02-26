package tests.rescala.testtools

import org.scalactic.source
import org.scalatest.Tag
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import reactives.operator.Interface

abstract class RETests extends munit.FunSuite {


  def multiEngined(block: Interface => Any): Unit = block(reactives.default)

}
