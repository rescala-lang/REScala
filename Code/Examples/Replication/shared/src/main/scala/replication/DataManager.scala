package replication

import kofre.base.{Id, Lattice}

class DataWrapper[T: Lattice](
    var combinedState: T
) {

}

class DataManager(replicaId: Id) {}
