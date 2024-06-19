package lofi_acl.sync

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonWriter}
import lofi_acl.crypto.PublicIdentity
import rdts.base.Uid

object JsoniterCodecs {
  given uidKeyCodec: JsonKeyCodec[rdts.base.Uid] = new JsonKeyCodec[Uid]:
    override def decodeKey(in: JsonReader): Uid             = Uid(in.readKeyAsString())
    override def encodeKey(uid: Uid, out: JsonWriter): Unit = out.writeKey(uid.delegate)

  given pubIdentityKeyCodec: JsonKeyCodec[lofi_acl.crypto.PublicIdentity] = new JsonKeyCodec[PublicIdentity]:
    override def decodeKey(in: JsonReader): PublicIdentity               = PublicIdentity(in.readKeyAsString())
    override def encodeKey(pubId: PublicIdentity, out: JsonWriter): Unit = out.writeKey(pubId.id)
}
