package rescala.engines

object Engines extends CommonEngines {
  implicit val default: SimpleEngine = synchron

  val all: List[SimpleEngine] = List(synchron, unmanaged)

  val locksweep: TEngine = null
  val parallellocksweep: TEngine = null

}
