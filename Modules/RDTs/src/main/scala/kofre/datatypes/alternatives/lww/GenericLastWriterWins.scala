package kofre.datatypes.alternatives.lww

import kofre.datatypes.ILastWriterWins

/** Lattice with the least-upper-bound defined by the timeStamp.
  * Timestamps must be unique, totally ordered, consistent with causal order.
  */
case class GenericLastWriterWins[Time, Value](timestamp: Time, payload: Value) extends ILastWriterWins[Time, Value]
