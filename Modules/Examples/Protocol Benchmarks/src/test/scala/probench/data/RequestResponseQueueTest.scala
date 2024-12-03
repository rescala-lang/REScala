package probench.data

import rdts.base.{Lattice, LocalUid}
import rdts.dotted.Dotted

import scala.util.Random

class RequestResponseQueueTest extends munit.ScalaCheckSuite {

  type CUT = RequestResponseQueue[String, String]

  def empty: CUT = RequestResponseQueue.empty

  test("add request works") {
    given LocalUid = LocalUid.predefined("id1")

    val queue    = empty
    val reqDelta = queue.request("one")
    val merged   = queue.merge(reqDelta)

    assertEquals(merged.requests.head.value, "one")
  }

  test("add requests merge out of order") {
    given LocalUid = LocalUid.predefined("id1")

    var queue = empty

    val deltas = (0 to 10).map { i =>
      val delta = queue.request(f"req $i")
      queue = queue.merge(delta)
      delta
    }

    for _ <- 0 to 100 do {
      assertEquals(Random.shuffle(deltas).foldLeft(empty)(Lattice.merge), queue)
    }
  }

  test("respond to single request") {
    given LocalUid = LocalUid.predefined("id1")

    val queue       = empty
    val reqDelta    = queue.request("one")
    val mergedQueue = queue.merge(reqDelta)
    val resDelta    = mergedQueue.respond(reqDelta.requests.head, "1")
    val merged: CUT = mergedQueue.merge(resDelta)

    assertEquals(merged.responsesTo(merged.requests.head).map(_.value), List("1"))
  }

  test("respond merge out of order") {
    given LocalUid = LocalUid.predefined("id1")

    var queue = empty

    val reqDeltas = (0 to 10).map { i =>
      val delta = queue.request(f"req $i")
      queue = queue.merge(delta)
      delta
    }

    val resDeltas = (0 to 10).map { i =>
      val delta = queue.respond(queue.requests(i), f"res $i")
      queue = queue.merge(delta)
      delta
    }

    val allDeltas = reqDeltas ++ resDeltas

    for _ <- 0 to 100 do {
      assertEquals(Random.shuffle(allDeltas).foldLeft(empty)(Lattice.merge), queue)
    }
  }

  test("one uncompleted, one completed") {
    given LocalUid = LocalUid.predefined("id1")

    var queue = empty

    def mod(f: CUT => CUT): CUT = {
      val delta = f(queue)
      queue = queue.merge(delta)
      delta
    }

    val req1 = mod(_.request("one"))
    val req2 = mod(_.request("two"))
    val res1 = mod(q => q.respond(q.requests.head, "1"))
    val res2 = mod(q => q.respond(q.requests(1), "2"))

    assertEquals(queue.requests.map(_.value).toList, List("one", "two"))
    assertEquals(queue.responses.map(_.value).toList, List("1", "2"))

    val requestOne = queue.requests.head

    val comp = mod(q => q.complete(q.requests.head))

    assertEquals(queue.requests.map(_.value).toList, List("two"))
    assertEquals(queue.responses.map(_.value).toList, List("2"))

    mod(q => q.respond(requestOne, "1"))

    assertEquals(queue.responses.map(_.value).toList, List("2"))
  }

}
