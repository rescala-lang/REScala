package rescala.test.concurrency

import rescala.signals.Signals
import rescala.signals.Var
import rescala.propagation.Turn
import scala.util.Random
import rescala.propagation.TurnFactory
import rescala.propagation.turns._

object PaperGlitchAppTest extends App {

  val lnOf2 = scala.math.log(2) // natural log of 2
  def log2(x: Double): Double = scala.math.log(x) / lnOf2
  def isPowerOf2(x: Int) = (x & (x - 1)) == 0

  // ============================================================================================================

  implicit val turnType = UnSynchronized // <-- change here for FUN

  val price = Var(3)
  val tax = price.map { p => p / 3 }
  val quantity = Var(1)
  val total = Signals.mapping(quantity, price, tax) { implicit t =>
    quantity.get * (price.get + tax.get)
  }

  // ============================================================================================================

  total.changed += { v =>
    if (isPowerOf2(v)) {
      println("Ok: " + v)
    } else {
      System.err.println("Bad: " + v)
    }
  }

  // ============================================================================================================

  Spawn {
    for (i <- 0 to 100000) price.set(3 * 2 << Random.nextInt(8))
  }
  Spawn {
    for (i <- 0 to 100000) quantity.set(2 << Random.nextInt(8))
  }
}
