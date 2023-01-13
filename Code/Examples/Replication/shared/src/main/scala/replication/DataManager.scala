package replication

import kofre.base.{Id, Lattice}
import kofre.time.Dots

class DataWrapper[T: Lattice](
    var combinedState: T,
    var currentContext: Dots
) {

}

class DataManager(replicaId: Id) {}
