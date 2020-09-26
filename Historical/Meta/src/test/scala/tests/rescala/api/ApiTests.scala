package tests.rescala.api

import org.scalatest.FunSuite
import rescala.api.Api

class ApiTests extends FunSuite {

  val api: Api = Api.synchronApi
  import api._

  test("basic api use") {
    val evt    = Evt[Int]()
    val mapped = mapE(evt)(_ * 2)
    val folded = fold(mapped)(0)((acc, v) => acc + v)

    assert(now(folded) === 0)

    fire(evt, 10)

    assert(now(folded) === 20)

    fire(evt, 11)

    assert(now(folded) === 42)

  }

}
