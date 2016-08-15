package tests.rescala

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit


class SomeTest() extends AssertionsForJUnit  {

  @Test def someTest(): Unit = {
    val v1 = rescala.Var(10)
    val s1 = v1.transform(_ + 10)(rescala.Engine)
  }

}
