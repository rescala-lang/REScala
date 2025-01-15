package lofi_acl.sync.acl.bft

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import lofi_acl.access.PermissionTree
import lofi_acl.crypto.{Ed25519Util, PublicIdentity}
import lofi_acl.sync.acl.bft.BftAcl.{Delegation, EncodedDelegation, opCodec}

import java.security.PrivateKey
import java.util.Base64
import scala.collection.mutable

type Signature = String

case class Acl(read: Map[PublicIdentity, PermissionTree], write: Map[PublicIdentity, PermissionTree]):
  def addPermissions(user: PublicIdentity, read: PermissionTree, write: PermissionTree): Acl =
    Acl(
      this.read.updatedWith(user) {
        case Some(oldRead) => Some(oldRead.merge(read))
        case None          => Some(read)
      },
      this.write.updatedWith(user) {
        case Some(oldWrite) => Some(oldWrite.merge(write))
        case None           => Some(write)
      }
    )

case class BftAcl(ops: Map[Signature, Delegation], heads: Set[Signature]) {
  def delegateAccess(
      delegator: PublicIdentity,
      delegatorKey: PrivateKey,
      delegatee: PublicIdentity,
      read: PermissionTree,
      write: PermissionTree
  ): (BftAcl, EncodedDelegation) =
    require(read <= write) // Write access implies read access! (Not really enforced)
    val op = Delegation(delegator, delegatee, read, write, parents = heads)
    require(isDelegationLegal(op))
    val opBytes     = writeToArray(op)
    val sig         = Ed25519Util.sign(opBytes, delegatorKey)
    val sigAsString = Base64.getEncoder.encodeToString(sig)
    (BftAcl(ops + (sigAsString -> op), Set(sigAsString)), EncodedDelegation(sig, opBytes))

  def isDelegationLegal(op: Delegation): Boolean =
    val referenceVersion = reconstruct(op.parents).get
    op.read <= referenceVersion.read.getOrElse(op.delegator, PermissionTree.empty) &&
    op.write <= referenceVersion.write.getOrElse(op.delegator, PermissionTree.empty)

  def receive(signature: Array[Byte], encodedOp: Array[Byte]): Either[Set[Signature], BftAcl] =
    val signatureAsString = Base64.getEncoder.encodeToString(signature)
    if ops.contains(signatureAsString) then return Right(this)
    readFromArray[Delegation](encodedOp) match
      case delegation @ Delegation(delegator, delegatee, read, write, parents) =>
        if !Ed25519Util.checkEd25519Signature(encodedOp, signature, delegator) then throw InvalidSignatureException
        // Write access implies read access!
        require(read <= write)
        // Check preceding ops are already applied
        val missing = parents.filterNot(ops.contains)
        if missing.nonEmpty then return Left(missing)
        // Track new op, remove heads that op references as predecessors and add new op as head
        Right(BftAcl(ops + (signatureAsString -> delegation), (heads -- parents) + signatureAsString))

  def reconstruct(heads: Set[Signature]): Option[Acl] =
    require(heads.forall(ops.contains))

    val visited   = mutable.Set.empty[Signature]
    val toMerge   = mutable.Stack.from(heads)
    var resultAcl = Acl(Map.empty, Map.empty)

    while toMerge.nonEmpty do {
      val next = toMerge.pop()
      if !visited.contains(next) then
        ops(next) match
          case Delegation(_, delegatee, read, write, parents) =>
            visited += next
            toMerge ++= parents.diff(visited)
            resultAcl = resultAcl.addPermissions(delegatee, read, write)
    }

    Some(resultAcl)
}

object BftAcl:
  given opCodec: JsonValueCodec[Delegation] = JsonCodecMaker.make(
    CodecMakerConfig.withAllowRecursiveTypes(true) // Required for PermissionTree
  )

  case class Delegation(
      delegator: PublicIdentity,
      delegatee: PublicIdentity,
      read: PermissionTree,
      write: PermissionTree,
      parents: Set[Signature]
  )

  case class EncodedDelegation(sig: Array[Byte], op: Array[Byte])

object InvalidSignatureException extends RuntimeException
