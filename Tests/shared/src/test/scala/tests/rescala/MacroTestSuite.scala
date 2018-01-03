package tests.rescala


import rescala.Infiltrator.assertLevel

class MacroTestSuite extends RETests {



  allEngines("signal Reevaluates The Expression"){ engine => import engine._
    val v = Var(0)
    var i = 1
    val s: Signal[Int] = Signal { v(): @unchecked; i }
    i = 2
    v.set(2)
    assert(s.now === 2)
  }

  allEngines("the Expression Is Not Evaluated Every Time Get Val Is Called"){ engine => import engine._
    var a = 10
    val s: Signal[Int] = Signal { 1 + 1 + a }
    assert(s.now === 12)
    a = 11
    assert(s.now === 12)
  }


  allEngines("simple Signal Returns Correct Expressions"){ engine => import engine._
    val s: Signal[Int] = Signal(1 + 1 + 1)
    assert(s.now === 3)
  }

  allEngines("the Expression Is Evaluated Only Once"){ engine => import engine._

    var a = 0
    val v = Var(10)
    val s1: Signal[Int] = Signal { a += 1; v() % 10 }


    assert(a === 1)
    v.set(11)
    assert(a === 2)
    v.set(21)
    assert(a === 3)
    assert(s1.now === 1)
  }

  allEngines("exclamation mark acces works"){ engine => import engine._

    val v = Var(List(1,2,3))
    val s1: Signal[List[Int]] = Signal { v.!.map(_ + 2) }
    val s2: Signal[List[Int]] = Signal { (!v).map(_ + 2) }

    assert(s1.now === List(3,4,5))
    assert(s2.now === List(3,4,5))

  }

  allEngines("handlers Are Executed"){ engine => import engine._

    var test = 0
    val v = Var(1)

    val s1 = Signal { 2 * v() }
    val s2 = Signal { 3 * v() }
    val s3 = Signal { s1() + s2() }

    s1.changed += { (_) => test += 1 }
    s2.changed += { (_) => test += 1 }
    s3.changed += { (_) => test += 1 }

    assert(test === 0)

    v.set(3)
    assert(test === 3)

  }

  allEngines("level Is Correctly Computed"){ engine => import engine._

    val v = Var(1)

    val s1 = Signal { 2 * v() }
    val s2 = Signal { 3 * v() }
    val s3 = Signal { s1() + s2() }

    assertLevel(v, 0)
    assertLevel(s1, 1)
    assertLevel(s2, 1)
    assertLevel(s3, 2)


  }


  allEngines("conversion Function With Argument In Signal"){ engine => import engine._

    var test = 0
    val e = Evt[Int]
    val s: Signal[Int] = Signal { 2 * e.latest(0).apply() }

    s.change += { _ => test += 1 }
    assert(s.now === 0)
    e.fire(2)
    assert(s.now === 4)
    e.fire(3)
    assert(s.now === 6)
    assert(test === 2)
  }


  allEngines("conversion Function Without Argument In Signal"){ engine => import engine._

    var test = 0
    val e = Evt[Int]
    val s: Signal[Option[Int]] = Signal { e.latestOption().apply() }

    s.change += { _ => test += 1 }
    assert(s.now === None)
    e.fire(2)
    assert(s.now === Some(2))
    e.fire(3)
    assert(s.now === Some(3))
    assert(test === 2)
  }


  allEngines("conversion Functions Work In Signals In Object Construction In Overriden Def"){ engine => import engine._
    // a previous macro implementation yielded wrong results for code of the
    // following form, see:
    // https://github.com/guidosalva/examples/pull/4/files#r11724000
    var test = 0
    var e = null: Evt[Int]
    var s = null: Signal[Int]

    abstract class A {def obj(): Unit }
    val a = new A {
      def obj() = new {
        val evt = Evt[Int]
        val sig: Signal[Int] = Signal { 2 * evt.latest(0).apply() }

        e = evt
        s = sig
      }
    }

    a.obj()
    s.change += { _ => test += 1 }
    assert(s.now === 0)
    e.fire(2)
    assert(s.now === 4)
    e.fire(3)
    assert(s.now === 6)
    assert(test === 2)
  }


  allEngines("signals Nested In Vars"){ engine => import engine._

    val a = Var(3)
    val b = Var(Signal(a()))
    val c = Signal(b()())

    assert(c.now === 3)
    a set 4
    assert(c.now === 4)
    b set Signal(5)
    assert(c.now === 5)

  }


  allEngines("nested Defined Signals"){ engine => import engine._
    val a = Var(3)
    val b = Signal {
      val c = Signal { a() }
      c()
    }

    assert(b.now === 3)
    a set 4
    assert(b.now === 4)
    a set 5
    assert(b.now === 5)
  }


  allEngines("use Of Inside Signal"){ engine => import engine._
    val outside = Var(1)
    val inside = Var(10)

    def sig = Signal { outside() }

    val testsig = Signal {
      def sig = Signal { inside() }
      sig()
    }

    assert(testsig.now === 10)
    outside set 2
    inside set 11
    assert(testsig.now === 11)
    assert(sig.now === 2)
  }

  allEngines("use Of Outside Signal"){ engine => import engine._
    val outside = Var(1)
    val inside = Var(10)

    def sig()(implicit turnSource: CreationTicket) = Signal { outside() }

    val testsig = Signal {
      {
        def sig = Signal { inside() }
        sig()
      }
      sig().apply()
    }

    assert(testsig.now === 1)
    outside set 2
    inside set 11
    assert(testsig.now === 2)
  }


  allEngines("case Classes And Objects"){ engine => import engine._
    // would fail due to https://issues.scala-lang.org/browse/SI-5467
    // if we didn't work around un-type-checking issues

    class A(val i: Int)
    case class B(j: Int) extends A(0)

    val v1 = Var(new A(1))
    val v2 = Var(new A(2))

    val sig = Signal {
      case class TraitCase(a: Trait)

      trait Trait
      case class IntCase(i: Int) extends Trait
      case class StringCase(s: String, t: TraitCase) extends Trait
      case object CaseObject extends Trait

      val instance: Trait = StringCase("", TraitCase(IntCase(2)))
      val i = instance match {
        case IntCase(i) => v1().i + i
        case StringCase(_, TraitCase(IntCase(i))) => v1().i + i
        case CaseObject => v2().i
      }

      case class C(s: String)(j: Int) extends A(j) { def a = 9 }
      case object D { def x = "value" }

      val j = v2() match {
        case C(_) => 0
        case B(j) => j
        case _ => 2
      }

      D.x + (i * j + C("")(0).a)
    }

    assert(sig.now == "value15")
    v2 set new A(3)
    assert(sig.now == "value15")
    v1 set new A(3)
    assert(sig.now == "value19")
    v2 set B(4)
    assert(sig.now == "value29")
    v1 set new A(5)
    assert(sig.now == "value37")
  }

  allEngines("lazy Values"){ engine => import engine._
    // would fail due to https://issues.scala-lang.org/browse/SI-5466
    // if we didn't work around un-type-checking issues

    val v1 = Var(4)
    val v2 = Var(false)

    val sig = Signal {
      lazy val v = v1()
      if (v2()) v else 0
    }

    assert(sig.now == 0)
    v1 set 5
    assert(sig.now == 0)
    v2 set true
    assert(sig.now == 5)
    v1 set 2
    assert(sig.now == 2)
    v2 set false
    assert(sig.now == 0)
    v1 set 8
    assert(sig.now == 0)
  }

  allEngines("pattern Matching And Wildcard"){ engine => import engine._
    // would fail due to https://issues.scala-lang.org/browse/SI-5465
    // if we didn't work around un-type-checking issues

    val v1 = Var(List(1, 2))
    val v2 = Var(100)

    val sig = Signal {
      v1() match {
        case List(_, v) => v
        case _ => v2()
      }
    }

    assert(sig.now == 2)
    v2 set 50
    assert(sig.now == 2)
    v1 set List(7, 8, 9)
    assert(sig.now == 50)
    v2 set 4
    assert(sig.now == 4)
    v1 set List(10, 11)
    assert(sig.now == 11)
  }


  allEngines("pattern Matching Anonymous Function"){ engine => import engine._
    val s1 = Signal { List(Some(1), Some(2), None, Some(4), None) }
    val s2 = Signal {
      s1() collect { case Some(n) => n }
    }
    assert(s2.now === List(1, 2, 4))
  }

  allEngines("pattern Matching Anonymous Function Nested Signals"){ engine => import engine._
    val v1 = Var(1)
    val v2 = Var(2)
    val s1 = Signal { List(Some(v1), None, Some(v2), None) }
    val s2 = Signal {
      s1() collect { case Some(n) => n() }
    }
    assert(s2.now === List(1, 2))
    v1.set(10)
    assert(s2.now === List(10, 2))
  }

  allEngines("abstract Type Member"){ engine =>
    // the renamed engines are a workaround for this bug: https://issues.scala-lang.org/browse/SI-10036
    // using the same engine in both trait and object causes the compiler to generate two field with the same name
    val engine1 = engine
    val engine2 = engine
    trait T {
      import engine1._
      type A
      val v: Var[A]
      val s = Signal { v() }
    }
    object o extends T {
      import engine2._
      type A = Int
      lazy val v = Var(4)
    }
    implicit val iengine = engine
    assert(o.s.now == 4)
  }

  allEngines("default Arguments"){ engine => import engine._
    val s = Signal {
      def a(v: Int, i: Int = 8, j: Int, k: Int = 8) = v + i + j + k
      a(6, j = 5)
    }
    assert(s.now == 27)
  }

  allEngines("outer And Inner Values"){ engine => import engine._
    val v = Var(0)
    object obj {
      def sig = Signal { v() }
    }

    val evt = Evt[Int]

    val testsig = Signal {
      val localsig = obj.sig
      val latest = evt latest -1

      localsig() + latest()
    }

    assert(testsig.now === -1)
    evt.fire(100)
    assert(testsig.now === 100)
    v set 10
    assert(testsig.now === 110)
    evt.fire(10)
    assert(testsig.now === 20)
    evt.fire(5)
    assert(testsig.now === 15)
    v set 50
    assert(testsig.now === 55)
  }


  allEngines("chained Signals1"){ engine => import engine._

    import scala.language.reflectiveCalls

    val v1 = Var { 1 }
    val v2 = Var { 2 }
    val v = Var { List(new {val s = v1 }, new {val s = v2 }) }

    val sig = Signal { v() map (_.s()) }

    assert(sig.now === List(1, 2))
    v1 set 5
    assert(sig.now === List(5, 2))
    v2 set 7
    assert(sig.now === List(5, 7))
    v set v.now.reverse
    assert(sig.now === List(7, 5))
  }

  allEngines("chained Signals2"){ engine => import engine._

    import scala.language.reflectiveCalls

    val v1 = Var { 20 }
    val v2 = Var { new {def signal = Signal { v1() } } }
    val v3 = Var { new {val signal = Signal { v2() } } }

    val s = Signal { v3() }

    val sig = Signal { s().signal().signal(): @unchecked }

    assert(sig.now === 20)
    v1 set 30
    assert(sig.now === 30)
    v2 set new {def signal = Signal { 7 + v1() } }
    assert(sig.now === 37)
    v1 set 10
    assert(sig.now === 17)
    v3 set new {val signal = Signal { new {def signal = Signal { v1() } } } }
    assert(sig.now === 10)
    v2 set new {def signal = Signal { 10 + v1() } }
    assert(sig.now === 10)
    v1 set 80
    assert(sig.now === 80)
  }


  allEngines("function As Getter For Signal"){ engine => import engine._

 import scala.language.reflectiveCalls

    def getSignal(obj: {def signal: Signal[Int]}) = obj.signal

    val v = Var { 20 }
    val o = new {val signal = Signal { v() } }

    val sig = Signal { getSignal(o)() }

    assert(sig.now === 20)
    v set 30
    assert(sig.now === 30)
  }


  allEngines("function As Getter For Event And Conversion Function"){ engine => import engine._

 import scala.language.reflectiveCalls

    def getSignal(obj: {def evt: Event[Int]}) = obj.evt

    val e = Evt[Int]
    val o = new {val evt = e }

    val sig = Signal { getSignal(o).latestOption().apply() }

    assert(sig.now === None)
    e.fire(30)
    assert(sig.now === Some(30))
  }

  allEngines("extracting Signal Side Effects"){ engine => import engine._
    val e1 = Evt[Int]
    def newSignal(): Signal[Int] = e1.count()

    val macroRes = Signal {
      newSignal()()
    }
    val normalRes = Signals.dynamic() { t: DynamicTicket =>
      t.depend(newSignal())
    }
    assert(macroRes.now === 0, "before, macro")
    assert(normalRes.now === 0, "before, normal")
    e1.fire(1)
    assert(macroRes.now === 1, "after, macro")
    assert(normalRes.now === 1, "after, normal")
    e1.fire(1)
    assert(macroRes.now === 2, "end, macro")
    assert(normalRes.now === 1, "end, normal")
  }


  allEngines("correctly replace ticket during macro expansion"){ engine => import engine._

    def wantsTicket(implicit ct: engine.CreationTicket, ct2: engine.CreationTicket) = {
      ct == ct2 && ct.isInnerTicket() && ct2.isInnerTicket()
    }

    val s = Signal { wantsTicket }

    assert(s.now === true)

  }

}
