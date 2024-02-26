package tests.rescala.testtools

import reactives.operator.Interface

abstract class RETests extends munit.FunSuite {


  def multiEngined(block: Interface => Any): Unit = block(reactives.default)

}
