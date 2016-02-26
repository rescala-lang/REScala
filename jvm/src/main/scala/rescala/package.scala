import rescala.engines.EngineImpl
import rescala.parrp.{Backoff, ParRPStruct, ParRP}

package object rescala extends EngineImpl[ParRPStruct.type, ParRP](new ParRP(new Backoff))
