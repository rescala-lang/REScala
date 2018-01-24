package tests.rescala.statics

import tests.rescala.util.RETests

class MacroTestSuite extends RETests {

  allEngines(".value acces works"){ engine => import engine._

    val v = Var(List(1,2,3))
    val s1: Signal[List[Int]] = Signal { v.value.map(_ + 2) }
    val s2: Signal[List[Int]] = Signal { v.apply.map(_ + 2) }

    assert(s1.now === List(3,4,5))
    assert(s2.now === List(3,4,5))

  }

  allEngines("conversion Function With Argument In Signal"){ engine => import engine._

    var test = 0
    val e = Evt[Int]
    val s: Signal[Int] = Signal { 2 * e.latest(0).apply() }

    s.changed += { _ => test += 1 }
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

    s.changed += { _ => test += 1 }
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
    s.changed += { _ => test += 1 }
    assert(s.now === 0)
    e.fire(2)
    assert(s.now === 4)
    e.fire(3)
    assert(s.now === 6)
    assert(test === 2)
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


  allEngines("correctly replace ticket during macro expansion"){ engine => import engine._

    def wantsTicket(implicit ct: engine.CreationTicket, ct2: engine.CreationTicket) = {
      ct == ct2 && ct.isInnerTicket() && ct2.isInnerTicket()
    }

    val s = Signal { wantsTicket }

    assert(s.now === true)

  }

}
