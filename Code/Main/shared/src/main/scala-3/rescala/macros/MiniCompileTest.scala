package rescala.macros

import rescala.default.*

def run() = {
  val a = Var(3)
  val b = Var(Signal(a()))
  val c = Signal.dynamic(b.value.value)

  assert(c.readValueOnce == 3)
  a set 4
  assert(c.readValueOnce == 4)
  b set Signal(5)
  assert(c.readValueOnce == 5)
}

