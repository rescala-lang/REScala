package benchmarks.lattices.delta.crdt

implicit def idFromString(s: String): rdts.base.Uid = rdts.base.Uid.predefined(s)
