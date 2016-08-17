package tests.rescala.reactivestreams

import org.reactivestreams.Publisher
import org.reactivestreams.tck.{PublisherVerification, TestEnvironment}
import org.scalatest.testng.TestNGSuiteLike
import rescala._
import rescala.reactivestreams.REPublisher
import tests.rescala.concurrency.Spawn

class RSPTestSuite extends PublisherVerification[Long](new TestEnvironment(500), 1000)  with TestNGSuiteLike {
  override def createPublisher(elements: Long): Publisher[Long] = {
    println(s"create publisher $elements")
    val e = Evt[Long]
    var remaining = elements
    val repub = REPublisher(e)
//    Spawn {
//      println(s"remaining $remaining")
//      Thread.sleep(50)
//      println(s"remaining $remaining")
//      while (remaining > 0) {
//        println(s"firing $remaining")
//        e.fire(remaining)
//        remaining -= 1
//      }
//      repub.signalComplete()
//    }
    repub
  }
  override def createFailedPublisher(): Publisher[Long] = null
}
