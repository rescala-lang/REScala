package tests.rescala.misc

import tests.rescala.testtools.RETests

class MacrosInteractingWithLanguageFeatures extends RETests {
  multiEngined { engine =>
    import engine.*

    test("pattern Matching Anonymous Function Nested Signals") {
      val v1 = Var(1)
      val v2 = Var(2)
      val s1 = Signal { List(Some(v1), None, Some(v2), None) }
      val s2 = Signal.dynamic {
        s1.value collect { case Some(n) => n.value }
      }
      assert(s2.readValueOnce === List(1, 2))
      v1.set(10)
      assert(s2.readValueOnce === List(10, 2))
    }

    test("pattern Matching Anonymous Function") {
      val s1 = Signal { List(Some(1), Some(2), None, Some(4), None) }
      val s2 = Signal {
        s1.value collect { case Some(n) => n }
      }
    }

    test("abstract Type Member") {
      // the renamed engines are a workaround for this bug: https://issues.scala-lang.org/browse/SI-10036
      // using the same engine in both trait and object causes the compiler to generate two field with the same name
      val engine1: engine.type = engine
      val engine2: engine.type = engine
      trait T {
        import engine1.*
        type A
        lazy val v: Var[A]
        val s = Signal { v.value }
      }
      object o extends T {
        import engine2.*
        type A = Int
        lazy val v: Var[Int] = Var(4)
      }
      assert(o.s.readValueOnce == 4)
    }

//    test("case Classes And Objects") {
//      // would fail due to https://issues.scala-lang.org/browse/SI-5467
//      // if we didn't work around un-type-checking issues
//
//      class A(val i: Int)
//      case class B(j: Int) extends A(0)
//
//      val v1 = Var(new A(1))
//      val v2 = Var(new A(2))
//
//      @scala.annotation.nowarn
//      val sig = Signal {
//        case class TraitCase(a: Trait)
//
//        trait Trait
//        case class IntCase(i: Int)                     extends Trait
//        case class StringCase(s: String, t: TraitCase) extends Trait
//        case object CaseObject                         extends Trait
//
//        val instance: Trait = StringCase("", TraitCase(IntCase(2)))
//        val i = instance match {
//          case IntCase(i)                           => v1().i + i
//          case StringCase(_, TraitCase(IntCase(i))) => v1().i + i
//          case CaseObject                           => v2().i
//        }
//
//        case class C(s: String)(j: Int) extends A(j) { def a = 9 }
//        case object D { def x = "value" }
//
//        val j = v2() match {
//          case C(_) => 0
//          case B(j) => j
//          case _    => 2
//        }
//
//        D.x + (i * j + C("")(0).a)
//      }
//
//      assert(sig.readValueOnce == "value15")
//      v2 set new A(3)
//      assert(sig.readValueOnce == "value15")
//      v1 set new A(3)
//      assert(sig.readValueOnce == "value19")
//      v2 set B(4)
//      assert(sig.readValueOnce == "value29")
//      v1 set new A(5)
//      assert(sig.readValueOnce == "value37")
//    }
  }
}
