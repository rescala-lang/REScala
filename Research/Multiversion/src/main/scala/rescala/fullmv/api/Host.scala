package rescala.fullmv.api

trait Host {
  val sgt: SerializationGraphTracking
  val taskPool: TaskPool
}
