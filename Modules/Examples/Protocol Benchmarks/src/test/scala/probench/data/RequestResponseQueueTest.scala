package probench.data

import rdts.base.{Lattice, LocalUid}
import rdts.dotted.Dotted

import scala.util.Random

class RequestResponseQueueTest extends munit.ScalaCheckSuite {

  type CUT = Dotted[RequestResponseQueue[String, String]]

  def empty: CUT = Dotted.empty

  test("add request works") {
    given LocalUid = LocalUid.predefined("id1")

    val queue    = empty
    val reqDelta = queue.mod(q => q.request("one"))
    val merged   = queue.merge(reqDelta)

    assertEquals(merged.data.requests.head.value, "one")
  }

  test("add requests merge out of order") {
    given LocalUid = LocalUid.predefined("id1")

    var queue = empty

    val deltas = (0 to 10).map { i =>
      val delta = queue.mod(q => q.request(f"req $i"))
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
    val reqDelta    = queue.mod(q => q.request("one"))
    val resDelta    = queue.merge(reqDelta).mod(q => q.respond(reqDelta.data.requests.head, "1"))
    val merged: CUT = queue.merge(reqDelta).merge(resDelta)

    assertEquals(merged.data.responsesTo(merged.data.requests.head).map(_.value), List("1"))
  }

  test("respond merge out of order") {
    given LocalUid = LocalUid.predefined("id1")

    var queue = empty

    val reqDeltas = (0 to 10).map { i =>
      val delta = queue.mod(q => q.request(f"req $i"))
      queue = queue.merge(delta)
      delta
    }

    val resDeltas = (0 to 10).map { i =>
      val delta = queue.mod(q => q.respond(q.requests(i), f"res $i"))
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

    val req1 = mod(_.mod(_.request("one")))
    val req2 = mod(_.mod(_.request("two")))
    val res1 = mod(_.mod(q => q.respond(q.requests.head, "1")))
    val res2 = mod(_.mod(q => q.respond(q.requests(1), "2")))

    assertEquals(queue.data.requests.map(_.value).toList, List("one", "two"))
    assertEquals(queue.data.responses.map(_.value).toList, List("1", "2"))

    val requestOne = queue.data.requests.head

    val comp = mod(_.mod(q => q.complete(q.requests.head)))

    assertEquals(queue.data.requests.map(_.value).toList, List("two"))
    assertEquals(queue.data.responses.map(_.value).toList, List("2"))

    mod(_.mod(q => q.respond(requestOne, "1")))

    assertEquals(queue.data.responses.map(_.value).toList, List("2"))
  }

}
