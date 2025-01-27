package probench.data

import probench.data.RequestResponseQueue.Req
import rdts.base.{Lattice, LocalUid}

import scala.util.Random

class RequestResponseQueueTest extends munit.ScalaCheckSuite {

  type CUT = RequestResponseQueue[String, String]

  def empty: CUT = RequestResponseQueue.empty

  test("add request works") {
    given LocalUid = LocalUid.predefined("id1")

    val queue    = empty
    val reqDelta = queue.request("one")
    val merged   = queue.merge(reqDelta)

    assertEquals(merged.requestsSorted.head.value, "one")
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
    val request     = mergedQueue.requestsSorted.head
    val resDelta    = mergedQueue.respond(request, "1")
    val merged: CUT = mergedQueue.merge(resDelta)

    assertEquals(merged.responseTo(request).map(_.value), Some("1"))
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
      val delta = queue.respond(queue.firstUnansweredRequest.get, f"res $i")
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

    val req1                    = mod(_.request("one"))
    val req2                    = mod(_.request("two"))
    val requestOne: Req[String] = queue.requestsSorted.head
    val res1                    = mod(q => q.respond(q.requestsSorted.head, "1"))
    val res2                    = mod(q => q.respond(q.requestsSorted(1), "2"))

    // assertEquals(queue.requests.values.map(_.value).toList, List())
    assertEquals(queue.firstUnansweredRequest, None)
    assertEquals(queue.responses.values.map(_.value).toList, List("1", "2"))

    val complete = mod(q => q.complete(requestOne))

    assertEquals(queue.requestsSorted.map(_.value), List("two"))
    assertEquals(queue.responses.values.map(_.value).toList, List("1", "2"))

    mod(q => q.respond(requestOne, "1")) // respond again

    assertEquals(queue.responses.values.map(_.value).toList, List("1", "2"))
//    assertEquals(queue.responses.values.map(_.value).toList, List("2"))
  }

}
