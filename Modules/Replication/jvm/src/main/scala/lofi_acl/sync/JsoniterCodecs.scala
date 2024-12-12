package lofi_acl.sync

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import lofi_acl.crypto.PublicIdentity
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.Signature
import rdts.base.Uid
import rdts.time.Dots

object JsoniterCodecs {
  given uidKeyCodec: JsonKeyCodec[rdts.base.Uid] = new JsonKeyCodec[Uid]:
    override def decodeKey(in: JsonReader): Uid             = Uid(in.readKeyAsString())
    override def encodeKey(uid: Uid, out: JsonWriter): Unit = out.writeKey(uid.delegate)

  given pubIdentityKeyCodec: JsonKeyCodec[lofi_acl.crypto.PublicIdentity] = new JsonKeyCodec[PublicIdentity]:
    override def decodeKey(in: JsonReader): PublicIdentity               = PublicIdentity(in.readKeyAsString())
    override def encodeKey(pubId: PublicIdentity, out: JsonWriter): Unit = out.writeKey(pubId.id)

  given signatureCodec: JsonValueCodec[Signature] = new JsonValueCodec[Signature]:
    override def decodeValue(in: JsonReader, default: Signature): Signature =
      val sigArray = in.readBase64AsBytes(Array.empty)
      if sigArray.isEmpty then null
      else Signature(sigArray)
    override def encodeValue(sig: Signature, out: JsonWriter): Unit =
      if sig == null then out.writeVal("")
      else out.writeBase64Val(sig.sig, true)
    override def nullValue: Signature = null

  given dotsCodec: JsonValueCodec[Dots] = JsonCodecMaker.make

  given messageJsonCodec[RDT: JsonValueCodec]: JsonValueCodec[MonotonicAclSyncMessage[RDT]] = JsonCodecMaker.make(
    CodecMakerConfig
      .withAllowRecursiveTypes(true) // Required for PermissionTree
      .withMapAsArray(true)
  )
}
