package tests.rescala.concurrency

import java.util.concurrent.ConcurrentLinkedQueue

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.{Signals, Var}

import scala.util.Random

class PaperGlitchAppTest extends AssertionsForJUnit {

  @Test def run(): Unit = {
    val lnOf2 = scala.math.log(2) // natural log of 2
    def log2(x: Double): Double = scala.math.log(x) / lnOf2
    def isPowerOf2(x: Int) = (x & (x - 1)) == 0

    // ============================================================================================================

    import rescala.Implicits.unmanaged // <-- change here for FUN

    val price = Var(3)
    val tax = price.map { p => p / 3 }
    val quantity = Var(1)
    val total = Signals.mapping(quantity, price, tax) { implicit t =>
      quantity.now * (price.now + tax.now)
    }

    // ============================================================================================================

    val glitches = new ConcurrentLinkedQueue[Int]()

    total.changed += { v => if (!isPowerOf2(v)) glitches.add(v) }

    // ============================================================================================================

    @volatile var cancelled = false

    Spawn {
      while (!cancelled) price.set(3 * 2 << Random.nextInt(8))
    }
    Spawn {
      while (!cancelled) quantity.set(2 << Random.nextInt(8))
    }

    Thread.sleep(1000)
    cancelled = true
    assert(glitches.size() > 0)
  }
}
