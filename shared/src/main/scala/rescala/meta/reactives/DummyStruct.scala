package rescala.meta.reactives

import rescala.graph.Struct

class DummyStruct extends Struct {
  /**
    * This struct is only used as a placeholder to prevent combination with different types of reactives.
    * No actual spore exists as propagation is handled through the meta graph
    */
  final override type SporeP[P, R] = Nothing
}
