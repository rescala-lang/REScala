import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.{Bottom, Lattice}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer
import replication.JsoniterCodecs.given

case class TaskRef(id: String)
case class TaskData(
    desc: String,
    done: Boolean = false
)
case class TodoRepState(list: Dotted[ReplicatedList[TaskRef]], entries: Map[String, LastWriterWins[Option[TaskData]]])

given Lattice[TodoRepState] = Lattice.derived
given Bottom[TodoRepState]  = Bottom.derived

given codecRGA: JsonValueCodec[DeltaBuffer[Dotted[ReplicatedList[TaskRef]]]] =
  JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
given codecLww: JsonValueCodec[DeltaBuffer[LastWriterWins[Option[TaskData]]]] = JsonCodecMaker.make

given JsonValueCodec[TodoRepState] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
