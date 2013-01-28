package scala.events.test

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.events.ImperativeEvent
import scala.events.behaviour.Signal
import org.scalatest.matchers.ShouldMatchers
import scala.events.scalareact

@RunWith(classOf[JUnitRunner])
class SignalSuite extends FunSuite with ShouldMatchers {

  test("EScala events can be accumulated with fold") {
    val e = new ImperativeEvent[Int]
    val accum: Signal[Int] = e.fold(0) { _ + _ }
    e(1)
    e(2)
    accum.getValue should be(3)
    e(3)
    accum.getValue should be(6)
  }

  test("Signals have an EScala changed event") {
    val e = new ImperativeEvent[Int]
    val accum: Signal[Int] = e.fold(0) { _ + _ }

    var mutableString = ""

    accum.changed += { a => mutableString = a.toString }
    e(1)
    mutableString should be("1")
    e(2)
    mutableString should be("3")
    e(3)
    mutableString should be("6")
  }

  test("Scala.react dataflow can be used to create signals") {
    val e = new ImperativeEvent[Int]
    val accum = e.fold(0) { _ + _ }

    val flowSignal = scalareact.Signal.flow("No occurence") { self =>
      self await accum
      self() = "First occurence"
      self awaitNext accum
      self() = "Second occurence"
    }

    flowSignal.getValue should be("No occurence")
    e(1)
    flowSignal.getValue should be("First occurence")
    e(1)
    flowSignal.getValue should be("Second occurence")
    e(1)
    flowSignal.getValue should be("Second occurence")
  }

  ignore("Scala.react turns get triggered at most 2 * [event invocation + signal instanciation]") {
    val e = new ImperativeEvent[Int]

    val eventCounter = e.iterate(0)(_ + 1)
    val accum = e.fold(0) { _ + _ }
    val dep1 = Signal { accum() + 1 }
    val dep2 = Signal { dep1() * 2 }

    e(1)
    e(2)
    e(3)

    val nEvents = 3
    val nRootSignals = 2
    val maxTurns = 2 * (nEvents + nRootSignals)
    assert(eventCounter.getValue === nEvents)

    val turns = scala.events.scalareact.engine.currentTurn.asInstanceOf[Int]
    turns should be <= (maxTurns)
  }

}