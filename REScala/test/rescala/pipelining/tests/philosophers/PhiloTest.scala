package rescala.pipelining.tests.philosophers

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.turns.{Engine, Engines, Turn}
import tests.rescala.concurrency.Spawn
import rescala.pipelining.tests.philosophers.PhilosopherTable.{Thinking, Seating, Taken}
import scala.annotation.tailrec
import scala.util.Random
import rescala.graph.Committable
import rescala.pipelining.tests.philosophers.PhilosopherTable.Free
import rescala.pipelining.tests.philosophers.PhilosopherTable.Ready
import rescala.pipelining.PipeliningTurn


class PhiloTest extends AssertionsForJUnit {

  @tailrec
  final def deal[A](deck: List[A], hands: List[List[A]]): List[List[A]] = deck match {
    case Nil => hands
    case card :: rest => deal(rest, hands.tail :+ (card :: hands.head))
  }


  def `eat!`(implicit engine: Engine[Turn]): Unit = {
    val philosophers = 4
    val threadCount = 4
    val table = new PhilosopherTable(philosophers, 0)(engine)
    val blocks: Array[Array[Seating]] = deal(table.seatings.toList, List.fill(threadCount)(Nil)).map(_.toArray).toArray

    @volatile var cancel = false
    @volatile var noException = true

    val threads = for (threadIndex <- Range(0, threadCount)) yield Spawn {
      while (!cancel) try {
       
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
              println(s"${Thread.currentThread().getId}: THinking assertions for ${t}")
              implicit val pt = t.asInstanceOf[PipeliningTurn]
              assert(seating.philosopher.outgoing.get(t) == Set(seating.leftFork, seating.rightFork))
              assert(seating.leftFork.outgoing.get(t).contains(seating.vision))
              assert(seating.rightFork.outgoing.get(t).contains(seating.vision))
              assert(seating.vision.incoming.get(t) == Set(seating.leftFork, seating.rightFork))
              assert(seating.vision.pipeline.needFrame().isWritten)
              assert(seating.philosopher(t) == Thinking)
              assert(seating.leftFork(t) == Free)
              assert(seating.rightFork(t) == Free)
              println(s"${Thread.currentThread().getId}: GET VISION for ${t}")
              assert(seating.vision(t) == Ready, s"${Thread.currentThread().getId} " + seating.vision.pipeline.getPipelineFrames().toString())
            }
          })
        })
        
        println(s"${thread}: ${seating.placeNumber} is thinkning again")
      } catch {
        case e : Exception => 
          noException = false
        
      }
    }

    println(s"philo party sleeping on $engine")
    Thread.sleep(1000)
    cancel = true
    threads.foreach(_.join())
    assert(noException)
    assert(threads.forall(!_.isAlive), "threads did not finish")
    println(s"philo party done sleeping on $engine")
  }

  @Test(timeout=10000) def eatingContestsPipelining(): Unit = `eat!`(Engines.pipelining)

}
