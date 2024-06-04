package tests.rescala.static.signals

import org.scalatest.freespec.AnyFreeSpec
import rescala.default._

class DefaultSignalMacro extends AnyFreeSpec {
  "default signal expression works" in {
    val s = Signal { 10 }
    assert(s.readValueOnce === 10)
  }
}
