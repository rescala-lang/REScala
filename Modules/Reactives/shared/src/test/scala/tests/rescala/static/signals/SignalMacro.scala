package tests.rescala.static.signals

import reactives.SelectedScheduler.candidate.State as BundleState
import reactives.core.{CreationScope, CreationTicket}
import tests.rescala.testtools.FunSuiteInvertedAssert

class SignalMacro extends FunSuiteInvertedAssert {
  import reactives.default.*
  {

    test("value access works") {

      val v                     = Var(List(1, 2, 3))
      val s1: Signal[List[Int]] = Signal { v.value.map(_ + 2) }
      val s2: Signal[List[Int]] = Signal { v.value.map(_ + 2) }

      assertEquals(s1.readValueOnce, List(3, 4, 5))
      assertEquals(s2.readValueOnce, List(3, 4, 5))

    }

    test("regression test for macro with more than 4 signals") {
      val source = Signal(1)

      Signal {
        (
          source.value,
          source.value,
          source.value,
          source.value,
          source.value
        )
      }
    }

    test("nested mapping over event") {
      var test = 0

      val e = Evt[Int]()
      val s = Signal { e }
      val r = s map { event => event map { _ + 1 } }

      r.readValueOnce observe { test = _ }
      assertEquals(test, 0)
      e.fire(2)
      assertEquals(test, 3)
      e.fire(4)
      assertEquals(test, 5)
    }

    test("conversion Function With Argument In Signal") {

      var test           = 0
      val e              = Evt[Int]()
      val s: Signal[Int] = Signal { 2 * e.hold(0).value }

      s.changed observe { _ => test += 1 }
      assertEquals(s.readValueOnce, 0)
      e.fire(2)
      assertEquals(s.readValueOnce, 4)
      e.fire(3)
      assertEquals(s.readValueOnce, 6)
      assertEquals(test, 2)
    }

    test("conversion Function Without Argument In Signal") {

      var test                   = 0
      val e                      = Evt[Int]()
      val s: Signal[Option[Int]] = Signal { e.holdOption().value }

      s.changed observe { _ => test += 1 }
      assertEquals(s.readValueOnce, None)
      e.fire(2)
      assertEquals(s.readValueOnce, Some(2))
      e.fire(3)
      assertEquals(s.readValueOnce, Some(3))
      assertEquals(test, 2)
    }

    test("conversion Functions Work In Signals In Object Construction In Overridden Def") {
      // a previous macro implementation yielded wrong results for code of the
      // following form, see:
      // https://github.com/guidosalva/examples/pull/4/files#r11724000
      var test = 0
      var e    = null: Evt[Int]
      var s    = null: Signal[Int]

      abstract class A { def obj(): Unit }
      val a = new A {
        def obj(): Unit = {
          new {
            val evt              = Evt[Int]()
            val sig: Signal[Int] = Signal { 2 * evt.hold(0).value }

            e = evt
            s = sig
          }
          ()
        }
      }

      a.obj()
      s.changed observe { _ => test += 1 }
      assertEquals(s.readValueOnce, 0)
      e.fire(2)
      assertEquals(s.readValueOnce, 4)
      e.fire(3)
      assertEquals(s.readValueOnce, 6)
      assertEquals(test, 2)
    }

    test("lazy Values") {
      // would fail due to https://issues.scala-lang.org/browse/SI-5466
      // if we didn't work around un-type-checking issues

      val v1 = Var(4)
      val v2 = Var(false)

      val sig = Signal {
        lazy val v = v1.value
        if v2.value then v else 0
      }

      assert(sig.readValueOnce == 0)
      v1 `set` 5
      assert(sig.readValueOnce == 0)
      v2 `set` true
      assert(sig.readValueOnce == 5)
      v1 `set` 2
      assert(sig.readValueOnce == 2)
      v2 `set` false
      assert(sig.readValueOnce == 0)
      v1 `set` 8
      assert(sig.readValueOnce == 0)
    }

    test("pattern Matching And Wildcard") {
      // would fail due to https://issues.scala-lang.org/browse/SI-5465
      // if we didn't work around un-type-checking issues

      val v1 = Var(List(1, 2))
      val v2 = Var(100)

      val sig = Signal {
        v1.value match {
          case List(_, v) => v
          case _          => v2.value
        }
      }

      assert(sig.readValueOnce == 2)
      v2 `set` 50
      assert(sig.readValueOnce == 2)
      v1 `set` List(7, 8, 9)
      assert(sig.readValueOnce == 50)
      v2 `set` 4
      assert(sig.readValueOnce == 4)
      v1 `set` List(10, 11)
      assert(sig.readValueOnce == 11)
    }

    test("default Arguments") {
      val s = Signal {
        def a(v: Int, i: Int = 8, j: Int, k: Int = 8) = v + i + j + k
        a(6, j = 5)
      }
      assert(s.readValueOnce == 27)
    }

    // test("function As Getter For Signal") {
    //
    //  import scala.language.reflectiveCalls
    //
    //  def getSignal(obj: { def signal: Signal[Int] }) = obj.signal
    //
    //  val v = Var { 20 }
    //  val o = new { val signal = Signal { v() } }
    //
    //  val sig = Signal.dynamic { getSignal(o)() }
    //
    //  assertEquals(sig.readValueOnce, 20)
    //  v `set` 30
    //  assertEquals(sig.readValueOnce, 30)
    // }

    // test("function As Getter For Event And Conversion Function") {
    //
    //  import scala.language.reflectiveCalls
    //
    //  def getSignal(obj: { def evt: Event[Int] }) = obj.evt
    //
    //  val e = Evt[Int]()
    //  val o = new { val evt = e }
    //
    //  val sig = Signal { getSignal(o).holdOption().apply() }
    //
    //  assertEquals(sig.readValueOnce, None)
    //  e.fire(30)
    //  assertEquals(sig.readValueOnce, Some(30))
    // }

    test("correctly replace ticket during macro expansion") {

      def wantsTicket(using
          ct: CreationTicket[BundleState],
          ct2: CreationTicket[BundleState]
      ): (Boolean, Boolean, Boolean) = {
        (
          ct.scope == ct2.scope,
          ct.scope.isInstanceOf[CreationScope.StaticCreationScope[BundleState]],
          ct2.scope.isInstanceOf[CreationScope.StaticCreationScope[BundleState]]
        )
      }

      val s = Signal { wantsTicket }

      assertEquals(s.readValueOnce, ((true, true, true)))

    }

    test("define force cut out at definition time with accessor") {

      val source = Var("Hallo")
      object myMap {
        var ms: Var[String] = source
      }

      val greeting = Signal {
        (myMap.ms).value
      }

      assertEquals(greeting.readValueOnce, source.readValueOnce)
      myMap.ms = Var("Something else")
      assertEquals(greeting.readValueOnce, source.readValueOnce)
      source.set("Welt")
      assertEquals(greeting.readValueOnce, source.readValueOnce)
    }

    test("define force cut out at definition time with def") {

      val source         = Var("Hallo")
      var indirectSource = source
      object myMap {
        def ms: Var[String] = indirectSource
      }

      val greeting = Signal {
        (myMap.ms).value
      }

      assertEquals(greeting.readValueOnce, source.readValueOnce)
      indirectSource = Var("Something else")
      assertEquals(greeting.readValueOnce, source.readValueOnce)
      source.set("Welt")
      assertEquals(greeting.readValueOnce, source.readValueOnce)
    }

    test("can generate signals in map") {

      val source  = Evt[String]()
      val mapping = Map("Hallo" -> Var("Welt"), "Test" -> Var("String"))

      val selected = source.map(mapping.get).flatten.hold().flatten(using Flatten.flattenImplicitForsignal)

      source.fire("Hallo")

      assertEquals(selected.readValueOnce, "Welt")

    }

  }
}
