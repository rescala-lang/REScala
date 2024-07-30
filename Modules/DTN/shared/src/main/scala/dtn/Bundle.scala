package dtn

import io.bullet.borer.derivation.ArrayBasedCodecs.deriveCodec
import io.bullet.borer.{Cbor, Codec, Decoder, Encoder, Reader}
import rdts.base.Uid
import rdts.time.{ArrayRanges, Dots, Time}

import java.nio.charset.StandardCharsets
import java.time.temporal.ChronoField
import java.time.{Duration, ZoneId, ZonedDateTime}
import java.util.Base64
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/* This module supports:
serialization: object -> CBOR
deserialization: CBOR -> object && JSON -> object

todo: for object -> JSON serialization the corresponding counter-method to readBytes(reader: Reader): Array[Byte] is missing. we would need info on the desired serialized presentation (JSON or CBOR) for this.
 */

enum RdtMessageType:
  case Request, Payload

case class Endpoint(scheme: Int, specific_part: String | Int) {
  def full_uri: String = {
    scheme match {
      case Endpoint.DTN_URI_SCHEME_ENCODED => specific_part match {
          case Endpoint.NONE_ENDPOINT_SPECIFIC_PART_ENCODED =>
            s"${Endpoint.DTN_URI_SCHEME_NAME}:${Endpoint.NONE_ENDPOINT_SPECIFIC_PART_NAME}"
          case _: Int    => throw Exception(s"unknown integer specific part: $specific_part")
          case _: String => s"${Endpoint.DTN_URI_SCHEME_NAME}:$specific_part"
        }
      case Endpoint.IPN_URI_SCHEME_ENCODED => s"${Endpoint.IPN_URI_SCHEME_NAME}:$specific_part"
      case _                               => throw Exception(s"unkown encoded dtn uri scheme: $scheme")
    }
  }

  def extract_node_endpoint(): Endpoint = {
    scheme match {
      case Endpoint.DTN_URI_SCHEME_ENCODED =>
        specific_part match
          case Endpoint.NONE_ENDPOINT_SPECIFIC_PART_ENCODED => Endpoint.NONE_ENDPOINT
          case _: Int => throw Exception(s"unkown integer specific part: $specific_part")
          case s: String =>
            val i: Int = s.indexOf("/", 2)
            if i == -1 then Endpoint(scheme, specific_part)
            else Endpoint(scheme, s.substring(0, i + 1))
      case Endpoint.IPN_URI_SCHEME_ENCODED => throw Exception(s"cannot extract node endpoint from ipn endpoint: $this")
      case _                               => throw Exception(s"unkown encoded dtn uri scheme: $scheme")
    }
  }

  def extract_node_name(): String = {
    scheme match {
      case Endpoint.DTN_URI_SCHEME_ENCODED =>
        specific_part match
          case Endpoint.NONE_ENDPOINT_SPECIFIC_PART_ENCODED =>
            Endpoint.NONE_ENDPOINT_SPECIFIC_PART_NAME.split("/")(2) // "none"
          case _: Int    => throw Exception(s"unkown integer specific part: $specific_part")
          case s: String => s.split("/")(2)
      case Endpoint.IPN_URI_SCHEME_ENCODED => throw Exception(s"cannot extract node name from ipn endpoint: $this")
      case _                               => throw Exception(s"unkown encoded dtn uri scheme: $scheme")
    }
  }

  def extract_endpoint_id(): String = {
    scheme match {
      case Endpoint.DTN_URI_SCHEME_ENCODED =>
        specific_part match
          case Endpoint.NONE_ENDPOINT_SPECIFIC_PART_ENCODED =>
            Endpoint.NONE_ENDPOINT_SPECIFIC_PART_NAME.split("/")(2) // "none"
          case _: Int => throw Exception(s"unkown integer specific part: $specific_part")
          case s: String =>
            val arr: Array[String] = s.split("/")
            if arr.length == 4 then arr(3) else ""
      case Endpoint.IPN_URI_SCHEME_ENCODED => throw Exception(s"cannot extract node name from ipn endpoint: $this")
      case _                               => throw Exception(s"unkown encoded dtn uri scheme: $scheme")
    }
  }
}
object Endpoint {
  val DTN_URI_SCHEME_NAME: String         = "dtn"
  val DTN_URI_SCHEME_ENCODED: Int         = 1
  val IPN_URI_SCHEME_NAME: String         = "ipn"
  val IPN_URI_SCHEME_ENCODED: Int         = 2
  val NONE_ENDPOINT_SPECIFIC_PART_NAME    = "//none"
  val NONE_ENDPOINT_SPECIFIC_PART_ENCODED = 0

  val NONE_ENDPOINT: Endpoint = Endpoint(DTN_URI_SCHEME_ENCODED, NONE_ENDPOINT_SPECIFIC_PART_ENCODED)

  def createFrom(full_uri: String): Endpoint = {
    val Array(scheme, specific_part) = full_uri.split(":", 2)

    scheme match {
      case DTN_URI_SCHEME_NAME => specific_part match {
          case NONE_ENDPOINT_SPECIFIC_PART_NAME => NONE_ENDPOINT
          case _                                => Endpoint(DTN_URI_SCHEME_ENCODED, specific_part)
        }
      case IPN_URI_SCHEME_NAME => Endpoint(IPN_URI_SCHEME_ENCODED, specific_part)
      case _                   => throw Exception(s"unknown dtn uri scheme: $scheme")
    }
  }

  def createFromName(node_name: String): Endpoint = Endpoint(DTN_URI_SCHEME_ENCODED, s"//$node_name/")
}

case class CreationTimestamp(bundle_creation_time: ZonedDateTime, sequence_number: Int) {
  def creation_time_as_dtn_timestamp: Long =
    Duration.between(CreationTimestamp.DTN_REFERENCE_TIMSTAMP, bundle_creation_time).toMillis
}
object CreationTimestamp {
  val DTN_REFERENCE_TIMSTAMP: ZonedDateTime = ZonedDateTime.of(2000, 1, 1, 0, 0, 0, 0, ZoneId.of("UTC"))

  private var old_now: Option[CreationTimestamp] = None

  def NOW: CreationTimestamp = {
    // DTN only uses millisecond resolution. So to avoid bundles with indistinguishable creation-timestamps we base our comparison on the already truncated timestamp.
    val untruncated_bundle_creation_time = ZonedDateTime.now(ZoneId.of("UTC"))
    val bundle_creation_time = untruncated_bundle_creation_time.`with`(
      ChronoField.NANO_OF_SECOND,
      (untruncated_bundle_creation_time.getNano / 1000000) * 1000000
    )
    var sequence_number = 0

    while old_now.isDefined && bundle_creation_time.isEqual(
        old_now.get.bundle_creation_time
      ) && sequence_number <= old_now.get.sequence_number
    do {
      sequence_number += 1
    }

    val now = CreationTimestamp(bundle_creation_time, sequence_number)
    old_now = Option(now)
    now
  }
}

case class FragmentInfo(fragment_offset: Int, total_application_data_length: Int)

case class HopCount(hop_limit: Int, current_count: Int)

case class RdtMetaInfo(dots: Dots, message_type: RdtMessageType)

case class PrimaryBlock(
    version: Int,
    bundle_processing_control_flags: BundleProcessingControlFlags,
    crc_type: Int,
    destination: Endpoint,
    source: Endpoint,
    report_to: Endpoint,
    creation_timestamp: CreationTimestamp,
    lifetime: Int = 1000 * 3600 * 24,
    var fragment_info: Option[FragmentInfo] = None,
    var crc: Option[Array[Byte]] = None
)

abstract class CanonicalBlock {
  def block_type_code: Int
  def block_number: Int
  def block_processing_control_flags: BlockProcessingControlFlags
  def crc_type: Int
  def data: Array[Byte]
  var crc: Option[Array[Byte]] = None
}
object CanonicalBlock {
  val PAYLOAD_BLOCK_TYPE_CODE: Int       = 1
  val PREVIOUS_NODE_BLOCK_TYPE_CODE: Int = 6
  val BUNDLE_AGE_BLOCK_TYPE_CODE: Int    = 7
  val HOP_COUNT_BLOCK_TYPE_CODE: Int     = 10
  val RDT_META_BLOCK_TYPE_CPDE: Int      = 201
}

case class PayloadBlock(
    block_type_code: Int,
    block_number: Int,
    block_processing_control_flags: BlockProcessingControlFlags,
    crc_type: Int,
    data: Array[Byte]
) extends CanonicalBlock {
  def payload_as_utf8: String = String(Base64.getDecoder.decode(data), StandardCharsets.UTF_8)
}
object PayloadBlock {
  def createFrom(data: Array[Byte]): PayloadBlock =
    PayloadBlock(CanonicalBlock.PAYLOAD_BLOCK_TYPE_CODE, 0, BlockProcessingControlFlags(), 0, data)
  def createFrom(utf8_payload: String): PayloadBlock = PayloadBlock(
    CanonicalBlock.PAYLOAD_BLOCK_TYPE_CODE,
    0,
    BlockProcessingControlFlags(),
    0,
    Base64.getEncoder.encode(utf8_payload.getBytes(StandardCharsets.UTF_8))
  )
}
case class PreviousNodeBlock(
    block_type_code: Int,
    block_number: Int,
    block_processing_control_flags: BlockProcessingControlFlags,
    crc_type: Int,
    data: Array[Byte]
) extends CanonicalBlock {
  def previous_node_id: Endpoint = Cbor.decode(data).to[Endpoint].value
}
object PreviousNodeBlock {
  def createFrom(node: Endpoint): PreviousNodeBlock = PreviousNodeBlock(
    CanonicalBlock.PREVIOUS_NODE_BLOCK_TYPE_CODE,
    0,
    BlockProcessingControlFlags(),
    0,
    Cbor.encode(node).toByteArray
  )
}
case class BundleAgeBlock(
    block_type_code: Int,
    block_number: Int,
    block_processing_control_flags: BlockProcessingControlFlags,
    crc_type: Int,
    data: Array[Byte]
) extends CanonicalBlock {
  def age_milliseconds: Int = Cbor.decode(data).to[Int].value
}
case class HopCountBlock(
    block_type_code: Int,
    block_number: Int,
    block_processing_control_flags: BlockProcessingControlFlags,
    crc_type: Int,
    data: Array[Byte]
) extends CanonicalBlock {
  def hop_count: HopCount = Cbor.decode(data).to[HopCount].value
}
object HopCountBlock {
  def createFrom(hop_count: HopCount): HopCountBlock = HopCountBlock(
    CanonicalBlock.HOP_COUNT_BLOCK_TYPE_CODE,
    0,
    BlockProcessingControlFlags(),
    0,
    Cbor.encode(hop_count).toByteArray
  )
}
case class UnknownBlock(
    block_type_code: Int,
    block_number: Int,
    block_processing_control_flags: BlockProcessingControlFlags,
    crc_type: Int,
    data: Array[Byte]
) extends CanonicalBlock

case class RdtMetaBlock(
    block_type_code: Int,
    block_number: Int,
    block_processing_control_flags: BlockProcessingControlFlags,
    crc_type: Int,
    data: Array[Byte]
) extends CanonicalBlock {
  def info: RdtMetaInfo = Cbor.decode(data).to[RdtMetaInfo].value
}
object RdtMetaBlock {
  def createFrom(info: RdtMetaInfo): RdtMetaBlock = RdtMetaBlock(
    CanonicalBlock.RDT_META_BLOCK_TYPE_CPDE,
    0,
    BlockProcessingControlFlags(),
    0,
    Cbor.encode(info).toByteArray
  )
}

case class Bundle(primary_block: PrimaryBlock, other_blocks: List[CanonicalBlock]) {
  def id: String =
    s"${primary_block.source.full_uri}-${primary_block.creation_timestamp.creation_time_as_dtn_timestamp}-${primary_block.creation_timestamp.sequence_number}"
}

abstract class Flags {
  protected def get(bit: Int, flags: Int): Boolean = ((flags >> bit) & 1) != 0
  protected def set(bit: Int, flags: Int): Int     = flags | 1 << bit
  protected def unset(bit: Int, flags: Int): Int   = flags & ~(1 << bit)
}

case class BundleProcessingControlFlags(
    is_fragment: Boolean = false,
    payload_is_admin_record: Boolean = false,
    do_not_fragment: Boolean = false,
    acknowledgement_is_requested: Boolean = false,
    status_time_is_requested: Boolean = false,
    status_report_of_reception_is_requested: Boolean = false,
    status_report_of_forwarding_is_requested: Boolean = false,
    status_report_of_delivery_is_requested: Boolean = false,
    status_report_of_deletion_is_requested: Boolean = false
)

case class BlockProcessingControlFlags(
    block_must_be_replicated_in_every_fragment: Boolean = false,
    report_status_if_block_cant_be_processed: Boolean = false,
    delete_bundle_if_block_cant_be_processed: Boolean = false,
    discard_block_if_block_cant_be_processed: Boolean = false
)

// utility method which either reads a byte-array (for CBOR) or an unbounded array of int (for JSON)
private def readBytes(reader: Reader): Array[Byte] = {
  if reader.hasByteArray then {
    reader.readByteArray()
  } else {
    val buffer: ArrayBuffer[Byte] = ArrayBuffer[Byte]()

    reader.readArrayStart()
    while !reader.hasBreak do {
      buffer += reader.readInt().toByte
    }
    reader.readArrayClose(unbounded = true, buffer.toArray)
  }
}

given Codec[RdtMessageType] = deriveCodec[RdtMessageType]
given Codec[RdtMetaInfo]    = deriveCodec[RdtMetaInfo]

given Encoder[ArrayRanges] = Encoder { (writer, arrayRanges) =>
  writer.write[Array[Time]](arrayRanges.inner.slice(0, arrayRanges.used))
}

given Decoder[ArrayRanges] = Decoder { reader =>
  val elems = reader.read[Array[Time]]()
  new ArrayRanges(
    inner = elems,
    used = elems.length
  )
}

given Codec[Uid]  = deriveCodec[Uid]
given Codec[Dots] = deriveCodec[Dots]

given Encoder[Endpoint] = Encoder { (writer, endpoint) =>
  writer
    .writeArrayOpen(2)
    .writeInt(endpoint.scheme)

  endpoint.specific_part match
    case normal_endpint: String => writer.writeString(normal_endpint)
    case none_endpoint: Int     => writer.writeInt(none_endpoint)

  writer.writeArrayClose()
}

given Decoder[Endpoint] = Decoder { reader =>
  // readArrayOpen(arity: Long) is JSON compatible, e.g. allows the serialized array to be unbounded
  val unbounded = reader.readArrayOpen(2)
  val endpoint = Endpoint(
    scheme = reader.readInt(),
    // the specific part can be a string to an dtn-endpoint or 0 to indicate the special dtn-endpoint "//none" (IPN not handled specifically, but endpoints are also encoded as string)
    specific_part = if reader.hasInt then reader.readInt() else reader.readString()
  )
  reader.readArrayClose(unbounded, endpoint)
}

given Encoder[CreationTimestamp] = Encoder { (writer, timestamp) =>
  writer
    .writeArrayOpen(2)
    .writeLong(timestamp.creation_time_as_dtn_timestamp)
    .writeInt(timestamp.sequence_number)
    .writeArrayClose()
}

given Decoder[CreationTimestamp] = Decoder { reader =>
  def from_dtn_timestamp(ts: Long): ZonedDateTime = CreationTimestamp.DTN_REFERENCE_TIMSTAMP.plus(Duration.ofMillis(ts))

  val unbounded = reader.readArrayOpen(2)
  val creation_timestamp = CreationTimestamp(
    bundle_creation_time = from_dtn_timestamp(reader.readLong()),
    sequence_number = reader.readInt()
  )
  reader.readArrayClose(unbounded, creation_timestamp)
}

given Encoder[FragmentInfo] = Encoder { (writer, fragmentInfo) =>
  writer
    .writeInt(fragmentInfo.fragment_offset)
    .writeInt(fragmentInfo.total_application_data_length)
}

given Decoder[FragmentInfo] = Decoder { reader =>
  FragmentInfo(
    fragment_offset = reader.readInt(),
    total_application_data_length = reader.readInt()
  )
}

given Encoder[HopCount] = Encoder { (writer, hopCount) =>
  writer
    .writeArrayOpen(2)
    .writeInt(hopCount.hop_limit)
    .writeInt(hopCount.current_count)
    .writeArrayClose()
}

given Decoder[HopCount] = Decoder { reader =>
  // readArrayOpen(arity: Long) is JSON compatible, e.g. allows the serialized array to be unbounded
  val unbounded = reader.readArrayOpen(2)
  val hop_count = HopCount(
    hop_limit = reader.readInt(),
    current_count = reader.readInt()
  )
  reader.readArrayClose(unbounded, hop_count)
}

given Encoder[BundleProcessingControlFlags] = Encoder { (writer, ctrlflags) =>
  def set(bit: Int, flags: Int): Int = flags | 1 << bit

  var flags: Int = 0

  if ctrlflags.is_fragment then flags = set(0, flags)
  if ctrlflags.payload_is_admin_record then flags = set(1, flags)
  if ctrlflags.do_not_fragment then flags = set(2, flags)
  // bits 3 and 4 are reserved for future use
  if ctrlflags.acknowledgement_is_requested then flags = set(5, flags)
  if ctrlflags.status_time_is_requested then flags = set(6, flags)
  // bits 7 to 13 are reserved for future use
  if ctrlflags.status_report_of_reception_is_requested then flags = set(14, flags)
  // bit 15 is reserved for future use
  if ctrlflags.status_report_of_forwarding_is_requested then flags = set(16, flags)
  if ctrlflags.status_report_of_delivery_is_requested then flags = set(17, flags)
  if ctrlflags.status_report_of_deletion_is_requested then flags = set(18, flags)
  // bits 19 and 20 are reserved for future use
  // bits 21 to 63 are unassigned

  writer.writeInt(flags)
}

given Decoder[BundleProcessingControlFlags] = Decoder { reader =>
  def get(bit: Int, flags: Int): Boolean = ((flags >> bit) & 1) != 0

  val flags: Int = reader.readInt()

  BundleProcessingControlFlags(
    is_fragment = get(0, flags),
    payload_is_admin_record = get(1, flags),
    do_not_fragment = get(2, flags),
    acknowledgement_is_requested = get(5, flags),
    status_time_is_requested = get(6, flags),
    status_report_of_reception_is_requested = get(14, flags),
    status_report_of_forwarding_is_requested = get(16, flags),
    status_report_of_delivery_is_requested = get(17, flags),
    status_report_of_deletion_is_requested = get(18, flags)
  )
}

given Encoder[PrimaryBlock] = Encoder { (writer, primaryBlock) =>
  // optional empty fields are not encoded, so we need to calculate our length beforehand
  var length = 8
  if primaryBlock.fragment_info.isDefined then length += 1
  if primaryBlock.crc.isDefined then length += 1

  writer
    .writeArrayOpen(length)
    .writeInt(primaryBlock.version)
    .write[BundleProcessingControlFlags](primaryBlock.bundle_processing_control_flags)
    .writeInt(primaryBlock.crc_type)
    .write[Endpoint](primaryBlock.destination)
    .write[Endpoint](primaryBlock.source)
    .write[Endpoint](primaryBlock.report_to)
    .write[CreationTimestamp](primaryBlock.creation_timestamp)
    .writeInt(primaryBlock.lifetime)

  if primaryBlock.fragment_info.isDefined then {
    writer.write[FragmentInfo](primaryBlock.fragment_info.get)
  }

  if primaryBlock.crc.isDefined then {
    writer.writeBytes(primaryBlock.crc.get)
  }

  writer.writeArrayClose()
}

given Decoder[PrimaryBlock] = Decoder { reader =>
  // the CBOR should always be bounded, e.g. has a length, but we do not know it beforehand
  // we sliently accept unbounded arrays because either the CBOR is not to spec, or we got a JSON encoded bundle where arrays are always unbounded
  val unbounded: Boolean = reader.hasArrayStart
  var length: Long       = -1
  if unbounded then { reader.readArrayStart() }
  else { length = reader.readArrayHeader() }

  val block = PrimaryBlock(
    version = reader.readInt(),
    bundle_processing_control_flags = reader.read[BundleProcessingControlFlags](),
    crc_type = reader.readInt(),
    destination = reader.read[Endpoint](),
    source = reader.read[Endpoint](),
    report_to = reader.read[Endpoint](),
    creation_timestamp = reader.read[CreationTimestamp](),
    lifetime = reader.readInt(),
  )

  // read fragment info
  if reader.hasInt then {
    block.fragment_info = Option(reader.read[FragmentInfo]())
  }

  // read crc
  if reader.hasByteArray || reader.hasArrayStart then { // be JSON compatible
    block.crc = Option(readBytes(reader))
  }

  reader.readArrayClose(unbounded, block)
}

given Encoder[BlockProcessingControlFlags] = Encoder { (writer, ctrlflags) =>
  def set(bit: Int, flags: Int): Int = flags | 1 << bit

  var flags: Int = 0

  if ctrlflags.block_must_be_replicated_in_every_fragment then flags = set(0, flags)
  if ctrlflags.report_status_if_block_cant_be_processed then flags = set(1, flags)
  if ctrlflags.delete_bundle_if_block_cant_be_processed then flags = set(2, flags)
  // bit 3 is reserved for future use
  if ctrlflags.discard_block_if_block_cant_be_processed then flags = set(4, flags)
  // bits 5 and 6 are reserved for future use
  // bits 7 to 63 are unassigned

  writer.writeInt(flags)
}

given Decoder[BlockProcessingControlFlags] = Decoder { reader =>
  def get(bit: Int, flags: Int): Boolean = ((flags >> bit) & 1) != 0

  val flags: Int = reader.readInt()

  BlockProcessingControlFlags(
    block_must_be_replicated_in_every_fragment = get(0, flags),
    report_status_if_block_cant_be_processed = get(1, flags),
    delete_bundle_if_block_cant_be_processed = get(2, flags),
    discard_block_if_block_cant_be_processed = get(4, flags)
  )
}

given Encoder[CanonicalBlock] = Encoder { (writer, block) =>
  // optional empty fields are not encoded, so we need to calculate our length beforehand
  var length = 5
  if block.crc.isDefined then length += 1

  writer
    .writeArrayOpen(length)
    .writeInt(block.block_type_code)
    .writeInt(block.block_number)
    .write[BlockProcessingControlFlags](block.block_processing_control_flags)
    .writeInt(block.crc_type)
    .writeBytes(block.data)

  if block.crc.isDefined then {
    writer.writeBytes(block.crc.get)
  }

  writer.writeArrayClose()
}

given Decoder[CanonicalBlock] = Decoder { reader =>
  // the CBOR should always be bounded, e.g. has a length, but we do not know it beforehand
  // we sliently accept unbounded arrays because either the CBOR is not to spec, or we got a JSON encoded bundle where arrays are always unbounded
  val unbounded: Boolean = reader.hasArrayStart
  var length: Long       = -1
  if unbounded then { reader.readArrayStart() }
  else { length = reader.readArrayHeader() }

  val block_type_code = reader.readInt()

  def readBlock(b_type: Int): CanonicalBlock = {
    b_type match
      case CanonicalBlock.PAYLOAD_BLOCK_TYPE_CODE => PayloadBlock(
          b_type,
          reader.readInt(),
          reader.read[BlockProcessingControlFlags](),
          reader.readInt(),
          readBytes(reader)
        )
      case CanonicalBlock.PREVIOUS_NODE_BLOCK_TYPE_CODE => PreviousNodeBlock(
          b_type,
          reader.readInt(),
          reader.read[BlockProcessingControlFlags](),
          reader.readInt(),
          readBytes(reader)
        )
      case CanonicalBlock.BUNDLE_AGE_BLOCK_TYPE_CODE => BundleAgeBlock(
          b_type,
          reader.readInt(),
          reader.read[BlockProcessingControlFlags](),
          reader.readInt(),
          readBytes(reader)
        )
      case CanonicalBlock.HOP_COUNT_BLOCK_TYPE_CODE => HopCountBlock(
          b_type,
          reader.readInt(),
          reader.read[BlockProcessingControlFlags](),
          reader.readInt(),
          readBytes(reader)
        )
      case CanonicalBlock.RDT_META_BLOCK_TYPE_CPDE => RdtMetaBlock(
          b_type,
          reader.readInt(),
          reader.read[BlockProcessingControlFlags](),
          reader.readInt(),
          readBytes(reader)
        )
      case _ => UnknownBlock(
          b_type,
          reader.readInt(),
          reader.read[BlockProcessingControlFlags](),
          reader.readInt(),
          readBytes(reader)
        )
  }

  val block = readBlock(block_type_code)

  if reader.hasByteArray || reader.hasArrayStart then { // be JSON compatible
    block.crc = Option(readBytes(reader))
  }

  reader.readArrayClose(unbounded, block)
}

given Encoder[Bundle] = Encoder { (writer, bundle) =>
  val length = 1 + bundle.other_blocks.size

  writer
    .writeArrayOpen(length)
    .write[PrimaryBlock](bundle.primary_block)

  for block <- bundle.other_blocks do {
    writer.write[CanonicalBlock](block)
  }

  writer.writeArrayClose()
}

given Decoder[Bundle] = Decoder { reader =>
  // the CBOR should always be bounded, e.g. has a length, but we do not know it beforehand
  // we sliently accept unbounded arrays because either the CBOR is not to spec, or we got a JSON encoded bundle where arrays are always unbounded
  val unbounded: Boolean = reader.hasArrayStart
  var length: Long       = -1
  if unbounded then { reader.readArrayStart() }
  else { length = reader.readArrayHeader() }

  val primaryBlock: PrimaryBlock = reader.read[PrimaryBlock]()

  val canonicalBlocks: ListBuffer[CanonicalBlock] = ListBuffer()

  while reader.hasArrayHeader || reader.hasArrayStart do {
    canonicalBlocks.addOne(reader.read[CanonicalBlock]())
  }

  reader.readArrayClose(unbounded, Bundle(primaryBlock, canonicalBlocks.toList))
}
