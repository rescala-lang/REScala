package tests.rescala.fullmv.transmitter

import org.scalatest.FunSuite
import rescala.fullmv.mirrors.SubsumableLockHostImpl
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.transmitter.SubsumableLockTransmittable
import retier.communicator.Requestor
import retier.communicator.tcp.TCP
import retier.registry.{Binding, Connections, Registry}

import retier.serializer.upickle._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

class SubsumableLockTransmitterTest extends FunSuite {
  class Host extends SubsumableLockHostImpl {
    val registry = new Registry

    implicit val lockTransmittable = SubsumableLockTransmittable.subsumableLockTransmittable(this)
    val binding = Binding[() => SubsumableLock]("makeLock")
    registry.bind(binding)(newLock _)

    def getRemote(requestor: Requestor[Connections.Protocol]): () => SubsumableLock = {
      val remoteConnection = Await.result(registry.request(requestor), Duration.Inf)
      val remoteFunction: () => Future[SubsumableLock] = registry.lookup(binding, remoteConnection)
      val res = { () =>
        Await.result(remoteFunction(), Duration.Inf)
      }
      res
    }

    def terminate(): Unit = registry.terminate()
  }

  class RemoteHost(port: Int) extends Host {
    registry.listen(TCP(port))
  }

  test("whatever") {
    val host0 = new Host
    import host0._
    try {

      val portA = 1099
      val hostA = new RemoteHost(portA)
      try {
        val remoteA = getRemote(TCP("localhost", portA))

        val portB = 1098
        val hostB = new RemoteHost(portB)
        try {
          val remoteB = getRemote(TCP("localhost", portA))

          val lock = newLock()
          val lockA = remoteA()
          val lockB = remoteB()

          val res = lock.tryLock()
          assert(res.success === true)
          val resA = lockA.trySubsume(res.newParent)
          assert(resA.isEmpty === true)
          res.newParent.unlock()

          val resB = lockB.tryLock()
          assert(resB.success === true)
          val res2 = lock.trySubsume(resB.newParent)
          assert(res2.isEmpty === true)

          val resA2 = lockA.tryLock()
          assert(resA2.success === false)
          assert(resA2.newParent === lockB)
        } finally {
          hostB.terminate()
        }
      } finally {
        hostA.terminate()
      }
    } finally {
      host0.terminate()
    }
  }
}
