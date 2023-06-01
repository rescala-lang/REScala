package example

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.Uid
import kofre.datatypes.contextual.{MultiVersionRegister, ReplicatedList}
import kofre.dotted.Dotted
import kofre.syntax.DeltaBuffer
import kofre.datatypes.contextual.LastWriterWins as LWW
import kofre.datatypes.GrowOnlyCounter as Counter
import kofre.syntax.ReplicaId

type ID = String

case class SocialMedia(sm: Map[ID, SocialPost]):
  def like(post: ID)(using replicaId: ReplicaId): SocialMedia =
    val increment = sm(post).likes.inc()
    SocialMedia(Map(post -> SocialPost(likes = increment)))

case class SocialPost(
    message: Option[LWW[String]] = None,
    comments: Set[LWW[String]] = Set.empty,
    likes: Counter = Counter.zero,
    dislikes: Counter = Counter.zero
)

class SocialMediaTest extends munit.FunSuite {}
