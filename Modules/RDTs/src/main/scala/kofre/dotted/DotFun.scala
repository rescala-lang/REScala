package kofre.dotted

import kofre.base.Lattice
import kofre.time.{Dot, Dots}

/** A dot fun tracks a set of values associated to a certain point in time.
  * This makes them useful as both [[kofre.datatypes.contextual.MultiVersionRegister]] and simple observe remove sets/maps.
  */
type DotFun[A] = Map[Dot, A]


