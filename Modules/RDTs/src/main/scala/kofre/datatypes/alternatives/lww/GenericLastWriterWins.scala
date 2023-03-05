package kofre.datatypes.alternatives.lww

/** Lattice with the least-upper-bound defined by the timeStamp.
  * Timestamps must be unique, totally ordered, consistent with causal order.
  */
case class GenericLastWriterWins[Time, Value](timestamp: Time, payload: Value) extends ILastWriterWins[Time, Value]
