package test.kofre.bespoke

import kofre.base.Uid
import kofre.datatypes.contextual.MultiVersionRegister
import kofre.datatypes.experiments.BoundedCounter
import kofre.dotted.Dotted
import kofre.syntax.TestReplica
import kofre.time.Dot
import test.kofre.DataGenerator.ExampleData
import test.kofre.given

class MultiVersionRegisterTest extends munit.FunSuite {

  test("basic usage") {

    val a = Dotted(MultiVersionRegister.empty[String])

    val alice   = Uid.predefined("alice")
    val bob     = Uid.predefined("bob")
    val charlie = Uid.predefined("charlie")

    val b = a.write(using alice)("hi")
    val c = a.write(using bob)("ho")

    val m1 = b merge c

    assertEquals(m1.read, Set("hi", "ho"))

    val d = m1.write(using charlie)("lets go!")

    assertEquals(d.read, Set("lets go!"))

    assertEquals(m1 merge d, d merge b)

  }

}
