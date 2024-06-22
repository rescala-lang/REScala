package test.rdts.bespoke

import rdts.base.Uid
import rdts.datatypes.contextual.MultiVersionRegister
import rdts.datatypes.experiments.BoundedCounter
import rdts.dotted.Dotted
import rdts.syntax.TestReplica
import rdts.time.Dot
import test.rdts.DataGenerator.ExampleData
import test.rdts.given

class MultiVersionRegisterTest extends munit.FunSuite {

  test("basic usage") {

    val a = Dotted(MultiVersionRegister.empty[String])

    val alice   = Uid.predefined("alice")
    val bob     = Uid.predefined("bob")
    val charlie = Uid.predefined("charlie")

    val b = a.mod(_.write(using alice)("hi"))
    val c = a.mod(_.write(using bob)("ho"))

    val m1 = b merge c

    assertEquals(m1.data.read, Set("hi", "ho"))

    val d = m1.mod(_.write(using charlie)("lets go!"))

    assertEquals(d.data.read, Set("lets go!"))

    assertEquals(m1 merge d, d merge b)

  }

}
