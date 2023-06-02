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

import rescala.default.*

type ID = String

object UI {
  object likeButton {
    def event: Event[Unit] = ???
  }
  object submitCommentButton {
    def event: Event[Unit] = ???
  }
  object postButton {
    def event: Event[Unit] = ???
  }
  val currentPostID: Signal[ID] = ???
  val textInput: Signal[String] = ???

  def display(ssm: Signal[SocialMedia]): Unit = ???

}

case class SocialMedia(sm: Map[ID, SocialPost] = Map.empty):
  def like(post: ID)(using replicaId: ReplicaId): SocialMedia =
    val increment = sm(post).likes.inc()
    SocialMedia(Map(post -> SocialPost(likes = increment)))

  def comment(post: ID, text: String)(using replicaId: ReplicaId): SocialMedia = ???
  def post(text: String)(using replicaId: ReplicaId): SocialMedia              = ???

case class SocialPost(
    message: Option[LWW[String]] = None,
    comments: Set[LWW[String]] = Set.empty,
    likes: Counter = Counter.zero,
    dislikes: Counter = Counter.zero
)

object SocialMediaTest {

  given ReplicaId = ReplicaId.fromId(Uid.gen())

  val likeEvent: Event[ID]        = UI.likeButton.event.snap { UI.currentPostID.value }
  val commentEvent: Event[String] = UI.submitCommentButton.event.snap { UI.textInput.value }
  val postEvent: Event[String]    = UI.postButton.event.snap { UI.textInput.value }

  val socialMedia: Signal[SocialMedia] =
    Fold(SocialMedia())(
      likeEvent act { id => current.like(id) },
      commentEvent act { text => current.comment(UI.currentPostID.value, text) },
      postEvent act { text => current.post(text) }
    )

  UI.display(socialMedia)

}
