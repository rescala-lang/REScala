package tests.rescala.concurrency.philosophers

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.graph.Spores
import rescala.synchronization.Engines
import rescala.turns.{Engine, Turn}
import tests.rescala.concurrency.Spawn
import tests.rescala.concurrency.philosophers.PhilosopherTable.{Thinking, Seating, Taken}
import scala.annotation.tailrec
import scala.util.Random
import tests.philosophers.REScalaPhilosophers.Eating
import rescala.graph.Committable
import tests.rescala.concurrency.philosophers.PhilosopherTable.Free
import tests.rescala.concurrency.philosophers.PhilosopherTable.Ready


class PhiloTest extends AssertionsForJUnit {

  @tailrec
  final def deal[A](deck: List[A], hands: List[List[A]]): List[List[A]] = deck match {
    case Nil => hands
    case card :: rest => deal(rest, hands.tail :+ (card :: hands.head))
  }


<<<<<<< HEAD:jvm/src/test/scala/tests/rescala/concurrency/philosophers/PhiloTest.scala
  def `eat!`[S <: Spores](engine: Engine[S, Turn[S]], dynamic: Boolean): Unit = {
    val philosophers = 5
    val threadCount = 3
    val table =
      if (!dynamic) new PhilosopherTable(philosophers, 0)(engine)
      else new DynamicPhilosopherTable(philosophers, 0)(engine)
    val blocks: Array[Array[Seating[S]]] = Array(table.seatings.toArray)
=======
  def `eat!`(implicit engine: Engine[Turn]): Unit = {
    val philosophers = 2
    val threadCount = 2
    val table = new PhilosopherTable(philosophers, 0)(engine)
    val blocks: Array[Array[Seating]] = deal(table.seatings.toList, List.fill(threadCount)(Nil)).map(_.toArray).toArray
>>>>>>> pipelining:REScala/test/tests/rescala/concurrency/philosophers/PhiloTest.scala

    @volatile var cancel = false
    @volatile var noException = true

<<<<<<< HEAD:jvm/src/test/scala/tests/rescala/concurrency/philosophers/PhiloTest.scala
    val threads = for (threadIndex <- Range(0, threadCount)) yield Spawn(name = s"Worker $threadIndex",  f = {
      while (!cancel) {
=======
    val threads = for (threadIndex <- Range(0, threadCount)) yield Spawn {
      while (!cancel) try {
       
>>>>>>> pipelining:REScala/test/tests/rescala/concurrency/philosophers/PhiloTest.scala
        val myBlock = blocks(threadIndex % blocks.length)
        val seating = myBlock(Random.nextInt(myBlock.length))
        val thread = Thread.currentThread().getId
        println(s"${thread}: ${seating.placeNumber} wants to eat")
        table.eatOnce(seating)
        println(s"${thread}: ${seating.placeNumber} ate once")
        table.engine.plan(seating.philosopher)(implicit t => {
          seating.philosopher.admit(Thinking)
          t.schedule(new Committable {
            override def release(implicit t : Turn) = {}
            override def commit(implicit t : Turn) = {
              assert(seating.philosopher(t) == Thinking)
              assert(seating.leftFork(t) == Free)
              assert(seating.rightFork(t) == Free)
              assert(seating.vision(t) == Ready)
            }
          })
        })
        
        println(s"${thread}: ${seating.placeNumber} is thinkning again")
      } catch {
        case e : Exception => 
          noException = false
        
      }
    })

    println(s"philo party sleeping on $engine (dynamic $dynamic)")
    Thread.sleep(2000)
    cancel = true
    threads.foreach(_.join())
    assert(noException)
    assert(threads.forall(!_.isAlive), "threads did not finish")
    println(s"philo party done sleeping on $engine (dynamic $dynamic)")
  }

<<<<<<< HEAD:jvm/src/test/scala/tests/rescala/concurrency/philosophers/PhiloTest.scala
  @Test def eatingContestsSpinning(): Unit = `eat!`(Engines.parrp, dynamic = false)

  @Test def eatingContestsSpinningDynamic(): Unit = `eat!`(Engines.parrp, dynamic = true)
=======
  @Test def eatingContestsSpinning(): Unit = `eat!`(Engines.spinning)
  @Test(timeout=10000) def eatingContestsPipelining(): Unit = `eat!`(Engines.pipelining)
>>>>>>> pipelining:REScala/test/tests/rescala/concurrency/philosophers/PhiloTest.scala

}
