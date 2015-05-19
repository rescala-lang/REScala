package tests.rescala.concurrency.philosophers

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.turns.{Engine, Engines, Turn}
import tests.rescala.concurrency.Spawn
import tests.rescala.concurrency.philosophers.PhilosopherTable.{Thinking, Seating}

import scala.annotation.tailrec
import scala.util.Random


class PhiloTest extends AssertionsForJUnit {

  @tailrec
  final def deal[A](deck: List[A], hands: List[List[A]]): List[List[A]] = deck match {
    case Nil => hands
    case card :: rest => deal(rest, hands.tail :+ (card :: hands.head))
  }


  def `eat!`(engine: Engine[Turn]): Unit = {
    val philosophers = 4
    val threadCount = 2
    val table = new PhilosopherTable(philosophers, 0)(engine)
    val blocks: Array[Array[Seating]] = deal(table.seatings.toList, List.fill(threadCount)(Nil)).map(_.toArray).toArray

    @volatile var cancel = false

    val threads = for (threadIndex <- Range(0, threadCount)) yield Spawn {
      while (!cancel) {
        val myBlock = blocks(threadIndex % blocks.length)
        val seating = myBlock(Random.nextInt(myBlock.length))
        val thread = Thread.currentThread().getId
        println(s"${thread}: ${seating.placeNumber} wants to eat")
        table.eatOnce(seating)
        println(s"${thread}: ${seating.placeNumber} ate once")
        seating.philosopher.set(Thinking)(table.engine)
        println(s"${thread}: ${seating.placeNumber} is thinkning again")
      }
    }

    println(s"philo party sleeping on $engine")
    Thread.sleep(1000)
    cancel = true
    threads.foreach(_.join())
    assert(threads.forall(!_.isAlive), "threads did not finish")
    println(s"philo party done sleeping on $engine")
  }

  @Test def eatingContestsSpinning(): Unit = `eat!`(Engines.spinning)
  @Test def eatingContestsPipelining(): Unit = `eat!`(Engines.pipelining)

}
