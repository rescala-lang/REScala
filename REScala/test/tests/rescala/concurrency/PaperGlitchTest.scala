package tests.rescala.concurrency

import java.util.concurrent.{ ConcurrentLinkedQueue, CountDownLatch }

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.{ Signals, Var }

import scala.util.Random

class PaperGlitchTest extends AssertionsForJUnit {

  @Test def run(): Unit = {
    val lnOf2 = scala.math.log(2) // natural log of 2
    def log2(x: Double): Double = scala.math.log(x) / lnOf2
    def isPowerOf2(x: Int) = (x & (x - 1)) == 0

    // ============================================================================================================

    // change here for FUN
    import rescala.turns.Engines.unmanaged

    val price = Var(3)
    val tax = price.map { p => p / 3 }
    val quantity = Var(1)
    val total = Signals.static(quantity, price, tax) { implicit t =>
      quantity.now * (price.now + tax.now)
    }

    // ============================================================================================================

    val glitches = new ConcurrentLinkedQueue[Int]()

    total.changed += { v => if (!isPowerOf2(v)) glitches.add(v) }

    // ============================================================================================================

    @volatile var cancelled = false

    val latch = new CountDownLatch(2)

    Spawn {
      latch.countDown()
      latch.await()
      while (!cancelled) {
        try {
          price.set(3 * 2 << Random.nextInt(8))
        } catch {
          case e: AssertionError => glitches.add(-42)
        }
      }
    }
    Spawn {
      latch.countDown()
      latch.await()
      while (!cancelled) {
        try {
          quantity.set(2 << Random.nextInt(8))
        } catch {
          case e: AssertionError => glitches.add(-42)
        }
      }
    }
    latch.await()

    Thread.sleep(1000)
    cancelled = true
    assert(glitches.size() > 0)
  }
}
