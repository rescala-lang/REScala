package tests.rescala.static.signals

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.reactives.PipelinedException
import tests.rescala.testtools.RETests

class ErrorPropagation extends RETests with ScalaCheckDrivenPropertyChecks with Matchers {
  multiEngined { engine => import engine._

    "EXPERIMENTAL" in forAll(Gen.posNum[Int]) { (n: Int) =>
      val e = Evt[Int]()
      val s: Signal[Int] = e.count

      var count = 0
      val t = s.changed.fold(Seq.empty[Int]) { (acc, c) =>  acc :+ c }

      val testSpec = Signal {
      print(s(), t().size)
      try {
        if(s() < 3) {
          assert(s() == t().size)
        } else {
          assert(s() > t().size)
        }
      }
      catch {
      case e: Throwable => throw PipelinedException(e)
      }
      }


      1 to n foreach { i => e.fire(i) }
      }
  }

}
