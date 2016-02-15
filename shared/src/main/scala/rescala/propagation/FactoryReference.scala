package rescala.propagation

import rescala.graph.Spores

abstract class FactoryReference[S <: Spores](override val bufferFactory: S) extends Turn[S]



