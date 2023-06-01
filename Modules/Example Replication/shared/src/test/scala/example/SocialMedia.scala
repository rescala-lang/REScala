package example

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.Uid
import kofre.datatypes.contextual.{MultiVersionRegister, ReplicatedList}
import kofre.dotted.Dotted
import kofre.syntax.DeltaBuffer

case class SocialMedia(sm: Map[Uid, SocialPost]):
  def like(post: ID, replica: ReplicaID): SocialMedia =
    val increment = sm(post).likes.inc(replica)
    SocialMedia(Map(post -> SocialPost(likes = increment)))

case class SocialPost(message: LWW[String], comments: Set[LWW[String]], likes: Counter, dislikes: Counter)

class SocialMediaTest extends munit.FunSuite {
}
