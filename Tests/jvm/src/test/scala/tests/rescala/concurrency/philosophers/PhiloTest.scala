package tests.rescala.concurrency.philosophers

import org.scalatest.FunSuite
import rescala.Engines
import rescala.engine.{Engine, Turn}
//import rescala.fullmv.FullMVEngine
import rescala.graph.Struct
import tests.rescala.concurrency.Spawn
import tests.rescala.concurrency.philosophers.PhilosopherTable.Thinking

import scala.annotation.tailrec


class PhiloTest extends FunSuite {

  @tailrec
  final def deal[A](deck: List[A], hands: List[List[A]]): List[List[A]] = deck match {
    case Nil => hands
    case card :: rest => deal(rest, hands.tail :+ (card :: hands.head))
  }


  def `eat!`[S <: Struct](engine: Engine[S, Turn[S]], dynamic: Boolean): Unit = {
    val size = 3
    val table =
      if (!dynamic) new PhilosopherTable(size, 0)(engine)
      else new DynamicPhilosopherTable(size, 0)(engine)

    @volatile var cancel = false

    val threads = for (threadIndex <- Range(0, size)) yield Spawn(desiredName = Some(s"Worker $threadIndex"), f = {
      while (!cancel) {
        val seating = table.seatings(Random.nextInt(size))
        table.eatOnce(seating)
        seating.philosopher.set(Thinking)(table.engine)
      }
    })

    println(s"philo party sleeping on $engine (dynamic $dynamic)")
    Thread.sleep(1000)
    cancel = true
    threads.foreach(_.join(1000))
    println(s"philo party done sleeping on $engine (dynamic $dynamic)")
  }


  test("eating Contests Spinning") {`eat!`(Engines.parrp, dynamic = false)}

  test("eating Contests Spinning Dynamic") {`eat!`(Engines.parrp, dynamic = true)}

  test("eating Contests Spinning Locksweep") {`eat!`(Engines.locksweep, dynamic = false)}

  test("eating Contests Spinning Dynamic Locksweep") {`eat!`(Engines.locksweep, dynamic = true)}

//  test("eating Contests Spinning FullMV") {`eat!`(FullMVEngine, dynamic = false)}

//  test("eating Contests Spinning Dynamic FullMV") {`eat!`(FullMVEngine, dynamic = true)}

  //  test("eatingContestsSpinningParallelLocksweep"){`eat!`(JVMEngines.parallellocksweep, dynamic = false)}
  //
  //  test("eatingContestsSpinningDynamicParallelLocksweep"){`eat!`(JVMEngines.parallellocksweep, dynamic = true)}

}
