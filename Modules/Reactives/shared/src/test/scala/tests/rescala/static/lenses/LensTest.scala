package tests.rescala.static.lenses

import reactives.extra.lenses.{AddLens, LVar, Lens, MulLens, SignalLens, toSignalLens}
import tests.rescala.testtools.RETests

class LensTest extends RETests {
  multiEngined { engine =>
    import engine._

    test("Bijective Lens Addition") {
      val a    = LVar[Int](3)
      val b    = a.applyLens(new AddLens[Int](3))
      val e1   = Evt[Int]()
      val test = Var(0)
      a.fire(e1)
      e1.fire(5)
      assert(a.now == 5)
      assert(b.now == 8)
      b.observe(value => test.set(value))
      assert(test.now == 8)
    }

    test("Bijective Lens Inversion") {
      val a  = LVar[Int](3)
      val b  = a.applyLens(new AddLens[Int](3))
      val c  = b.applyLens(new AddLens[Int](3).inverse)
      val e1 = Evt[Int]()
      a.fire(e1)
      e1.fire(5)
      assert(a.now == 5)
      assert(b.now == 8)
      assert(c.now == a.now)
    }

    test("Bijective Lens Composition") {
      val a  = LVar(3.0)
      val b  = a.applyLens(new AddLens(3.0).compose(new MulLens(3.0)))
      val e1 = Evt[Double]()
      a.fire(e1)
      e1.fire(5.0)
      assert(a.now == 5.0)
      assert(b.now == 24.0)
    }

    test("Bijective SignalLens Addition") {
      val a    = LVar[Int](3)
      val lens = Var(new AddLens(3))
      val b    = a.applyLens(SignalLens { Signal { lens.value } })
      val e1   = Evt[Int]()
      a.fire(e1)
      e1.fire(5)
      assert(a.now == 5)
      assert(b.now == 8)
      lens.set(new AddLens[Int](5))
      assert(a.now == 5)
      assert(b.now == 10)
    }

    class testType(val intValue: Int, val stringValue: String)

    class testLens extends Lens[testType, Int] {
      def toView(m: testType): Int = m.intValue + 3

      def toModel(v: Int, m: testType): testType =
        testType(v - 3, m.stringValue) // Record("intValue" -> v, "stringValue" -> m.stringValue).asInstanceOf[testType]
    }

    test("Non-Bijective Lens ") {
      val a =
        LVar[testType](testType(
          5,
          "THISDOESNOTCHANGE"
        )) // (Record("intValue" -> 5, "stringValue" -> "THISDOESNTCHANGE").asInstanceOf[testType])
      val b    = a.applyLens(testLens())
      val e1   = Evt[Int]()
      val test = Var(0)
      b.fire(e1)
      e1.fire(5)
      assert(a.now.intValue == 2)
      assert(a.now.stringValue == "THISDOESNOTCHANGE")
      assert(b.now == 5)
    }
  }
}
