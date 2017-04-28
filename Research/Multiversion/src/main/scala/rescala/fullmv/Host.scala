package rescala.fullmv

trait Host {
  val sgt: SerializationGraphTracking
  val taskPool: TaskPool
}
