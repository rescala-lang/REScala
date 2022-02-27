package kofre.primitives

import kofre.Lattice
import kofre.Defs

case class LastWriterWins[A](timestamp: Defs.Time, payload: A):
  def map[B](f: A => B): LastWriterWins[B] = LastWriterWins(f(payload))

object LastWriterWins:
  def apply[A](value: A): LastWriterWins[A] = LastWriterWins(System.currentTimeMillis(), value)
  // Note, this is incorrect for equal timestamps
  given lattice[A]: Lattice[LastWriterWins[A]] = (left, right) => if (right.timestamp > left.timestamp) right else left
