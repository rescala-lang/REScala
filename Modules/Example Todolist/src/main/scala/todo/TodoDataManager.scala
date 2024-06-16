package todo

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.{Bottom, Lattice}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer
import rdts.time.Dots
import reactives.default.*
import replication.DataManager

object TodoDataManager {

  case class TodoRepState(list: Dotted[ReplicatedList[TaskRef]], entries: Map[String, LastWriterWins[Option[TaskData]]])

  given JsonValueCodec[TodoRepState] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given Lattice[TodoRepState]        = Lattice.derived
  given Bottom[TodoRepState]         = Bottom.derived

  val dataManager = DataManager[TodoRepState](Todolist.replicaId)

  val lock                             = new Object
  var globalBuffer: List[TodoRepState] = Nil

  def publish[A](reactive: Signal[DeltaBuffer[A]], wrap: A => TodoRepState) = {
    reactive.map { v =>
      lock.synchronized {
        globalBuffer = globalBuffer.reverse_:::(v.deltaBuffer.map(wrap))
      }

    }.observe { _ =>
      lock.synchronized {
        if globalBuffer.nonEmpty then
          dataManager.applyUnrelatedDelta(globalBuffer.reduce(Lattice.merge))
      }
    }

  }

}
