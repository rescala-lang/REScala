package lofi_acl.sync.acl.monotonic

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import lofi_acl.crypto.{Ed25519Util, PublicIdentity}
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.AclDelta
import lofi_acl.sync.{InvalidMessageException, MessageSerialization}
import rdts.time.Dot

import java.io.{DataInputStream, DataOutputStream}
import java.security.{PrivateKey, SignatureException}
import java.util.Base64

class SignatureVerifyingMessageSerialization[RDT](
    localIdentity: PublicIdentity,
    signingKey: PrivateKey
)(using
    JsonValueCodec[MonotonicAclSyncMessage[RDT]]
) extends MessageSerialization[MonotonicAclSyncMessage[RDT]] {
  private val base64Decoder = Base64.getDecoder

  override def writeToStream(msg: MonotonicAclSyncMessage[RDT], outputStream: DataOutputStream): Unit =
    msg match
      case aclMsg: AclDelta[RDT] => writeSigned(aclMsg, outputStream)
      case _                        => writeUnsigned(msg, outputStream)

  override def readFromStream(inputStream: DataInputStream): MonotonicAclSyncMessage[RDT] =
    if inputStream.readBoolean()   // 1 byte
    then readSigned(inputStream)   /* Signed message */
    else readUnsigned(inputStream) /* Unsigned message */

  private def readSigned(inputStream: DataInputStream): AclDelta[RDT] = {
    val signature = Array.ofDim[Byte](64)
    require(inputStream.read(signature, 0, 64) == 64) // 64 bytes
    val msgLength = inputStream.readInt() // 4 bytes
    val msgBytes  = Array.ofDim[Byte](msgLength)
    require(inputStream.read(msgBytes, 0, msgLength) == msgLength) // n-bytes with n == length of the message

    val deserializedMsg = readFromArray[MonotonicAclSyncMessage[RDT]](msgBytes)

    deserializedMsg match
      case aclEntry @ AclDelta(_, _, _, Dot(author, _), _, _) => // Authorship is derived from dot
        if !Ed25519Util.checkEd25519Signature(msgBytes, signature, PublicIdentity.fromUid(author))
        then throw SignatureException("Failed to verify signature of received message")
        // Splice (verified) signature into object (null in serialized version so signature doesn't depend on itself)
        aclEntry.copy(signature = signature)
      case _ => throw InvalidMessageException("Signed message is not an update to ACL")
  }

  private def readUnsigned(inputStream: DataInputStream): MonotonicAclSyncMessage[RDT] = {
    val msgLength = inputStream.readInt()
    val msgBytes  = Array.ofDim[Byte](msgLength)
    require(inputStream.read(msgBytes, 0, msgLength) == msgLength)
    val deserializedMessage = readFromArray(msgBytes)
    if deserializedMessage.isInstanceOf[AclDelta[RDT]] // Should be signed
    then throw InvalidMessageException("Expected AddAclEntry message to be signed")
    deserializedMessage
  }

  private def writeSigned(msg: AclDelta[RDT], outputStream: DataOutputStream): Unit =
    require(msg.signature != null && msg.signature.length == 64)
    val msgBytes = writeToArray(msg.copy(signature = null))
    outputStream.writeBoolean(true /* signature following */ ) // 1 byte
    outputStream.write(msg.signature)                          // 64 bytes
    outputStream.writeInt(msgBytes.length)                     // 4 bytes
    outputStream.write(msgBytes)                               // n-bytes with n == length of the message

  private def writeUnsigned(msg: MonotonicAclSyncMessage[RDT], outputStream: DataOutputStream): Unit =
    val msgBytes = writeToArray(msg)
    outputStream.writeBoolean(false /* No signature */ ) // 1 byte
    outputStream.writeInt(msgBytes.length)               // 4 bytes
    outputStream.write(msgBytes)                         // n-bytes with n == length of the message
}
