package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.parrp.ParRP
import rescala.reactives.Var


class SomeTest() extends AssertionsForJUnit with MockitoSugar {

  @Test def someTest(): Unit = {
    val v1 = rescala.Var(10)
    val s1 = v1.transform(_ + 10)(rescala.Engine)
  }

}
