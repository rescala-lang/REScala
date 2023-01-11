package replication

import kofre.base.{Id, Lattice}

class DataWrapper[T: Lattice](var data: T)

class DataManager(replicaId: Id) {
}
