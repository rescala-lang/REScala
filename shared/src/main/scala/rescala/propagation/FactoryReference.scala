package rescala.propagation

import rescala.graph.Struct

abstract class FactoryReference[S <: Struct](override val bufferFactory: S) extends Turn[S]



