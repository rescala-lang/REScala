package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn

import scala.util.{Success, Try}

object ExceptionPropagationTestSuite extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class ExceptionPropagationTestSuite[S <: Struct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine.{Event, Evt, Signal, Var}


  @Test def basicSignalExceptions(): Unit = {
    val v = Var(42)
    val ds = Signal { 100 / v() }
    val ss = v.map(v => 100 / v)

    assert(ds.now == 100 / v.now, "dynamic arithmetic error")
    assert(ss.now == 100 / v.now, "static arithmetic error")

    v.set(0)

    intercept[ArithmeticException](ds.now)
    intercept[ArithmeticException](ss.now)

  }

  @Test def basicEventExcepitons(): Unit = {
    val e = Evt[Int]()
    val de = Event { e().map(100./) }
    val se = e.map(v => 100 / v)

    var dres: Try[Int] = null
    var sres: Try[Int] = null

    de.toTry().observe(dres = _)
    se.toTry().observe(sres = _)

    e.fire(42)

    assert(dres === Success(100 / 42), "dynamic arithmetic error")
    assert(sres === Success(100 / 42), "static arithmetic error")

    e.fire(0)

    intercept[ArithmeticException](dres.get)
    intercept[ArithmeticException](sres.get)

  }


  @Test def moreExceptions(): Unit = {
    val input = Evt[String]()
    val trimmed = input.map(_.trim)
    val toInted = trimmed.map(_.toInt)
    val folded = toInted.fold(100)((acc, v) => acc / v)
    val `change'd` = folded.change.toTry()

    var res: Try[(Int, Int)] = null

    `change'd`.observe(res = _)

    assert(folded.now === 100)

    input.fire("10")
    assert(folded.now === 10, "successful fold")
    assert(res === Success((100, 10)), "successful changed")

    input.fire(" 2  ")
    assert(folded.now === 5, "successful fold 2")
    assert(res === Success((10, 5)), "successful changed 2")

    input.fire(" 0  ")
    intercept[ArithmeticException](folded.now)
    intercept[ArithmeticException](res.get)

//    input.fire(" aet ")
//    intercept[ArithmeticException](folded.now)
//    intercept[ArithmeticException](res.get)

  }

  @Test def signalRegenerating(): Unit = {
    val input = Var("100")
    val trimmed = input.map(_.trim)
    val folded = trimmed.map(_.toInt)
    val `change'd` = folded.change.toTry()

    var res: Try[(Int, Int)] = null

    `change'd`.observe(res = _)



    assert(folded.now === 100)

    input.set("10")
    assert(folded.now === 10, "successful fold")
    assert(res === Success((100, 10)), "successful changed")

    input.set(" 2  ")
    assert(folded.now === 2, "successful fold 2")
    assert(res === Success((10, 2)), "successful changed2")


    input.set(" 0  ")
    assert(folded.now === 0, "successful fold 3")
    assert(res === Success((2, 0)), "successful changed3")

    input.set(" aet ")
    intercept[NumberFormatException](folded.now)
    intercept[NumberFormatException](res.get)


    input.set("100")
    assert(folded.now === 100, "successful fold 5")
    intercept[NumberFormatException](res.get) //TODO: should maybe change?

    input.set("200")
    assert(res === Success((100, 200)), "successful changed3")







  }

}
