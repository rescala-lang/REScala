package tests.rescala.misc

import tests.rescala.testtools.RETests

class UDFSyntaxTest extends RETests {
  multiEngined { engine =>
    import engine._
    import UserDefinedFunction._

    test("experiment With Implicit Syntax") {

      val input   = Var("Hello")
      val macroed = Signals.ofUDF(s"${input.value} Macro Implicits!")
      val noMacro =
        Signals.ofUDF(new UserDefinedFunction(Set(input), dt => s"${dt.dependStatic(input)} no macros no implicits!"))

      assert(macroed.readValueOnce === "Hello Macro Implicits!")
      assert(noMacro.readValueOnce === "Hello no macros no implicits!")

      input.set("Yay")

      assert(macroed.readValueOnce === "Yay Macro Implicits!")
      assert(noMacro.readValueOnce === "Yay no macros no implicits!")

    }

  }
}
