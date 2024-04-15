package test.rdts.baseproperties

import lofi_acl.DataGenerator.arbCounter
import lofi_acl.ardt.datatypes.{AddWinsLastWriterWinsMap, AddWinsMap, Counter}

class CounterChecks extends LatticePropertyChecks[Counter](false)(using arbCounter, Counter.lattice)
