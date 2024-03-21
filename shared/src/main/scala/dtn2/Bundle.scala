package dtn2

import io.bullet.borer.{Cbor, Decoder, Encoder}

import scala.collection.mutable.ListBuffer
import java.util.Base64
import java.nio.charset.StandardCharsets


case class Endpoint(scheme: Int, specific_part: String | Int)

case class CreationTimestamp(bundle_creation_time: Long, sequence_number: Int)

case class FragmentInfo(fragment_offset: Int, total_application_data_length: Int)

case class HopCount(hop_limit: Int, current_count: Int)

case class PrimaryBlock(version: Int,
                        bundle_processing_control_flags: ReadonlyBundleProcessingControlFlags,
                        crc_type: Int,
                        destination: Endpoint,
                        source: Endpoint,
                        report_to: Endpoint,
                        creation_timestamp: CreationTimestamp,
                        lifetime: Int = 1000 * 3600 * 24,
                        var fragment_info: Option[FragmentInfo] = None,
                        var crc: Option[Array[Byte]] = None)

abstract class CanonicalBlock {
  def block_type_code: Int
  def block_number: Int
  def block_processing_control_flags: ReadonlyBlockProcessingControlFlags
  def crc_type: Int
  def data: Array[Byte]
  var crc: Option[Array[Byte]] = None
}

case class PayloadBlock(block_type_code: Int, block_number: Int, block_processing_control_flags: ReadonlyBlockProcessingControlFlags, crc_type: Int, data: Array[Byte]) extends CanonicalBlock {
  def payload_as_utf8: String = String(Base64.getDecoder.decode(data), StandardCharsets.UTF_8)
}
case class PreviousNodeBlock(block_type_code: Int, block_number: Int, block_processing_control_flags: ReadonlyBlockProcessingControlFlags, crc_type: Int, data: Array[Byte]) extends CanonicalBlock {
  def previous_node_id: Endpoint = Cbor.decode(data).to[Endpoint].value
}
case class BundleAgeBlock(block_type_code: Int, block_number: Int, block_processing_control_flags: ReadonlyBlockProcessingControlFlags, crc_type: Int, data: Array[Byte]) extends CanonicalBlock {
  def age_milliseconds: Int = Cbor.decode(data).to[Int].value
}
case class HopCountBlock(block_type_code: Int, block_number: Int, block_processing_control_flags: ReadonlyBlockProcessingControlFlags, crc_type: Int, data: Array[Byte]) extends CanonicalBlock {
  def hop_count: HopCount = Cbor.decode(data).to[HopCount].value
}
case class UnknownBlock(block_type_code: Int, block_number: Int, block_processing_control_flags: ReadonlyBlockProcessingControlFlags, crc_type: Int, data: Array[Byte]) extends CanonicalBlock

case class Bundle(primary_block: PrimaryBlock, other_blocks: List[CanonicalBlock])


abstract class Flags(var _flags: Int) {
  protected def get(bit: Int): Boolean = ((_flags >> bit) & 1) != 0
  protected def set(bit: Int): Unit = _flags |= 1 << bit
  protected def unset(bit: Int): Unit = _flags &= ~(1 << bit)
}

class ReadonlyBundleProcessingControlFlags(flags: Int) extends Flags(flags) {
  def is_fragment: Boolean = get(0)
  def payload_is_admin_record: Boolean = get(1)
  def do_not_fragment: Boolean = get(2)
  // bits 3 and 4 are reserved for future use
  def acknowledgement_is_requested: Boolean = get(5)
  def status_time_is_requested: Boolean = get(6)
  // bits 7 to 13 are reserved for future use
  def status_report_of_reception_is_requested: Boolean = get(14)
  // bit 15 is reserved for future use
  def status_report_of_forwarding_is_requested: Boolean = get(16)
  def status_report_of_delivery_is_requested: Boolean = get(17)
  def status_report_of_deletion_is_requested: Boolean = get(18)
  // bits 19 and 20 are reserved for future use
  // bits 21 to 63 are unassigned
  def asMutableFlags: MutableBundleProcessingControlFlags = MutableBundleProcessingControlFlags(_flags)

  override def toString(): String = {
    "RoBuFlags{"
      + "is_fragment:" + is_fragment + ", "
      + "payload_is_admin_record:" + payload_is_admin_record + ", "
      + "do_not_fragment:" + do_not_fragment + ", "
      + "acknowledgement_is_requested:" + acknowledgement_is_requested + ", "
      + "status_time_is_requested:" + status_time_is_requested + ", "
      + "status_report_of_reception_is_requested:" + status_report_of_reception_is_requested + ", "
      + "status_report_of_forwarding_is_requested:" + status_report_of_forwarding_is_requested + ", "
      + "status_report_of_delivery_is_requested:" + status_report_of_delivery_is_requested + ", "
      + "status_report_of_deletion_is_requested:" + status_report_of_deletion_is_requested + "}"
  }
}

class MutableBundleProcessingControlFlags(flags: Int = 0) extends ReadonlyBundleProcessingControlFlags(flags: Int) {
  def is_fragment_=(makeSet: Boolean) = if(makeSet) set(0) else unset(0)
  def payload_is_admin_record_=(makeSet: Boolean) = if(makeSet) set(1) else unset(1)
  def do_not_fragment_=(makeSet: Boolean) = if(makeSet) set(2) else unset(2)
  // bits 3 and 4 are reserved for future use
  def acknowledgement_is_requested_=(makeSet: Boolean) = if(makeSet) set(5) else unset(5)
  def status_time_is_requested_=(makeSet: Boolean) = if(makeSet) set(6) else unset(6)
  // bits 7 to 13 are reserved for future use
  def status_report_of_reception_is_requested_=(makeSet: Boolean) = if(makeSet) set(14) else unset(14)
  // bit 15 is reserved for future use
  def status_report_of_forwarding_is_requested_=(makeSet: Boolean) = if(makeSet) set(16) else unset(16)
  def status_report_of_delivery_is_requested_=(makeSet: Boolean) = if(makeSet) set(17) else unset(17)
  def status_report_of_deletion_is_requested_=(makeSet: Boolean) = if(makeSet) set(18) else unset(18)
  // bits 19 and 20 are reserved for future use
  // bits 21 to 63 are unassigned
  def asReadonlyFlags: ReadonlyBundleProcessingControlFlags = ReadonlyBundleProcessingControlFlags(_flags)
}

class ReadonlyBlockProcessingControlFlags(flags: Int) extends Flags(flags) {
  def block_must_be_replicated_in_every_fragment: Boolean = get(0)
  def report_status_if_block_cant_be_processed: Boolean = get(1)
  def delete_bundle_if_block_cant_be_processed: Boolean = get(2)
  // bit 3 is reserved for future use
  def discard_block_if_block_cant_be_processed: Boolean = get(4)
  // bits 5 and 6 are reserved for future use
  // bits 7 to 63 are unassigned
  def asMutableFlags: MutableBlockProcessingControlFlags = MutableBlockProcessingControlFlags(_flags)

  override def toString(): String = {
    "RoBoFlags{"
      + "block_must_be_replicated_in_every_fragment:" + block_must_be_replicated_in_every_fragment + ", "
      + "report_status_if_block_cant_be_processed:" + report_status_if_block_cant_be_processed + ", "
      + "delete_bundle_if_block_cant_be_processed:" + delete_bundle_if_block_cant_be_processed + ", "
      + "discard_block_if_block_cant_be_processed:" + discard_block_if_block_cant_be_processed + "}"
  }
}

class MutableBlockProcessingControlFlags(flags: Int = 0) extends Flags(flags) {
  def block_must_be_replicated_in_every_fragment_=(makeSet: Boolean) = if(makeSet) set(0) else unset(0)
  def report_status_if_block_cant_be_processed_=(makeSet: Boolean) = if(makeSet) set(1) else unset(1)
  def delete_bundle_if_block_cant_be_processed_=(makeSet: Boolean) = if(makeSet) set(2) else unset(2)
  // bit 3 is reserved for future use
  def discard_block_if_block_cant_be_processed_=(makeSet: Boolean) = if(makeSet) set(4) else unset(4)
  // bits 5 and 6 are reserved for future use
  // bits 7 to 63 are unassigned
  def asReadonlyFlags: ReadonlyBlockProcessingControlFlags = ReadonlyBlockProcessingControlFlags(_flags)
}



given Encoder[Endpoint] = Encoder { (writer, endpoint) => 
  writer
    .writeArrayOpen(2)
    .writeInt(endpoint.scheme)
  
  endpoint.specific_part match
    case normal_endpint: String => writer.writeString(normal_endpint)
    case none_endpoint: Int => writer.writeInt(none_endpoint)
  
  writer.writeArrayClose()
}

given Decoder[Endpoint] = Decoder { reader =>
  val unbounded = reader.readArrayOpen(2)
  val endpoint = Endpoint(
    scheme = reader.readInt(),
    // the specific part can be a string to an dtn-endpoint or 0 to indicate the special dtn-endpoint "//none" (IPN not handled specifically, but endpoints are also encoded as string)
    specific_part = if (reader.hasInt) reader.readInt() else reader.readString()
  )
  reader.readArrayClose(unbounded, endpoint)
}



given Encoder[CreationTimestamp] = Encoder { (writer, timestamp) => 
  writer
    .writeArrayOpen(2)
    .writeLong(timestamp.bundle_creation_time)
    .writeInt(timestamp.sequence_number)
    .writeArrayClose()
}

given Decoder[CreationTimestamp] = Decoder { reader =>
  val unbounded = reader.readArrayOpen(2)
  val creation_timestamp = CreationTimestamp(
    bundle_creation_time = reader.readLong(),
    sequence_number = reader.readInt()
  )
  reader.readArrayClose(unbounded, creation_timestamp)
}



given Encoder[FragmentInfo] = Encoder { (writer, fragmentInfo) => 
  writer
    .writeArrayOpen(2)
    .writeInt(fragmentInfo.fragment_offset)
    .writeInt(fragmentInfo.total_application_data_length)
    .writeArrayClose()
}

given Decoder[FragmentInfo] = Decoder { reader =>
  val unbounded = reader.readArrayOpen(2)
  val fragment_info = FragmentInfo(
    fragment_offset = reader.readInt(),
    total_application_data_length = reader.readInt()
  )
  reader.readArrayClose(unbounded, fragment_info)
}



given Encoder[HopCount] = Encoder { (writer, hopCount) =>
  writer
    .writeArrayOpen(2)
    .writeInt(hopCount.hop_limit)
    .writeInt(hopCount.current_count)
    .writeArrayClose()
}

given Decoder[HopCount] = Decoder { reader =>
  val unbounded = reader.readArrayOpen(2)
  val hop_count = HopCount(
    hop_limit = reader.readInt(),
    current_count = reader.readInt()
  )
  reader.readArrayClose(unbounded, hop_count)
}



given Encoder[PrimaryBlock] = Encoder { (writer, primaryBlock) => 
  // optional empty fields are not encoded, so we need to calculate our length beforehand
  var length = 8
  if (primaryBlock.fragment_info.isDefined) length += 1
  if (primaryBlock.crc.isDefined) length += 1

  writer
    .writeArrayOpen(length)
    .writeInt(primaryBlock.version)
    .write[ReadonlyBundleProcessingControlFlags](primaryBlock.bundle_processing_control_flags)
    .writeInt(primaryBlock.crc_type)
    .write[Endpoint](primaryBlock.destination)
    .write[Endpoint](primaryBlock.source)
    .write[Endpoint](primaryBlock.report_to)
    .write[CreationTimestamp](primaryBlock.creation_timestamp)
    .writeInt(primaryBlock.lifetime)
  
  if (primaryBlock.fragment_info.isDefined) {
    writer.write[FragmentInfo](primaryBlock.fragment_info.get)
  }

  if (primaryBlock.crc.isDefined) {
    writer.writeBytes(primaryBlock.crc.get)
  }

  writer.writeArrayClose()
}

given Decoder[PrimaryBlock] = Decoder { reader =>
  // this is always bounded, e.g. has a length
  // but we don't care about the length because both fragment-info and crc are optional -> we parse by order of appearance
  // emulate: reader.readArrayOpen(pre-defined-length)
  val length = reader.readArrayHeader()
  val unbounded = false

  val block = PrimaryBlock(
    version = reader.readInt(),
    bundle_processing_control_flags = reader.read[ReadonlyBundleProcessingControlFlags](), 
    crc_type = reader.readInt(),
    destination = reader.read[Endpoint](), 
    source = reader.read[Endpoint](), 
    report_to = reader.read[Endpoint](),
    creation_timestamp = reader.read[CreationTimestamp](),
    lifetime = reader.readInt(),
  )

  // read fragment info
  if (reader.hasArrayStart) {
    block.fragment_info = Option(reader.read[FragmentInfo]())
  }

  // idk if fragment data is in a tuple or two plain values
  // in case of two plain values parse it like this
  if (reader.hasInt) {
    println("warning: I was wrong about fragment encoding")
    val fragment_info = FragmentInfo(
      fragment_offset = reader.readInt(),
      total_application_data_length = reader.readInt()
    )
    block.fragment_info = Option(fragment_info)
  }

  // read crc
  if (reader.hasByteArray) {
    block.crc = Option(reader.readByteArray())
  }

  reader.readArrayClose(unbounded, block)
}



given Encoder[CanonicalBlock] = Encoder { (writer, block) =>
  // optional empty fields are not encoded, so we need to calculate our length beforehand
  var length = 5
  if (block.crc.isDefined) length += 1

  writer
    .writeArrayOpen(length)
    .writeInt(block.block_type_code)
    .writeInt(block.block_number)
    .write[ReadonlyBlockProcessingControlFlags](block.block_processing_control_flags)
    .writeInt(block.crc_type)
    .writeBytes(block.data)
  
  if (block.crc.isDefined) {
    writer.writeBytes(block.crc.get)
  }

  writer.writeArrayClose()
}

given Decoder[CanonicalBlock] = Decoder { reader =>
  // this is always bounded, e.g. has a length
  // but we don't care about the length because crc is optional -> we parse by order of appearance
  val length = reader.readArrayHeader()
  val unbounded = false

  val block_type_code = reader.readInt()

  def readBlock(b_type: Int): CanonicalBlock = {
    b_type match
      case 1 => PayloadBlock(b_type, reader.readInt(), reader.read[ReadonlyBlockProcessingControlFlags](), reader.readInt(), reader.readByteArray())
      case 6 => PreviousNodeBlock(b_type, reader.readInt(), reader.read[ReadonlyBlockProcessingControlFlags](), reader.readInt(), reader.readByteArray())
      case 7 => BundleAgeBlock(b_type, reader.readInt(), reader.read[ReadonlyBlockProcessingControlFlags](), reader.readInt(), reader.readByteArray())
      case 10 => HopCountBlock(b_type, reader.readInt(), reader.read[ReadonlyBlockProcessingControlFlags](), reader.readInt(), reader.readByteArray())
      case _ => UnknownBlock(b_type, reader.readInt(), reader.read[ReadonlyBlockProcessingControlFlags](), reader.readInt(), reader.readByteArray())
  }
  
  val block = readBlock(block_type_code)

  if (reader.hasByteArray) {
    block.crc = Option(reader.readByteArray())
  }

  reader.readArrayClose(unbounded, block)
}



given Encoder[Bundle] = Encoder { (writer, bundle) => 
  val length = 1 + bundle.other_blocks.size

  writer
    .writeArrayOpen(length)
    .write[PrimaryBlock](bundle.primary_block)
  
  for (block <- bundle.other_blocks) {
    writer.write[CanonicalBlock](block)
  }

  writer.writeArrayClose()
}

given Decoder[Bundle] = Decoder { reader =>
  var unbounded = true
  if (reader.hasArrayStart) {
    // in case of not-to-spec-bundle with unbounded array, parse anyway
    println("warning: parsing not-to-spec bundle -> unbounded top level array")
    reader.readArrayStart()
  } else {
    // in case of bounded to-spec array -> emulate reader.readArrayOpen(pre-defined-length)
    // we parse by order of appearance and don't care about the length
    val length = reader.readArrayHeader()
    unbounded = false
  }

  val primaryBlock: PrimaryBlock = reader.read[PrimaryBlock]()

  val canonicalBlocks: ListBuffer[CanonicalBlock] = ListBuffer()

  while (reader.hasArrayHeader) {
    canonicalBlocks.addOne(reader.read[CanonicalBlock]())
  }

  reader.readArrayClose(unbounded, Bundle(primaryBlock, canonicalBlocks.toList))
}



given Encoder[ReadonlyBundleProcessingControlFlags] = Encoder { (writer, flags) =>
  writer.writeInt(flags._flags)
}

given Decoder[ReadonlyBundleProcessingControlFlags] = Decoder { reader => 
  ReadonlyBundleProcessingControlFlags(reader.readInt())
}



given Encoder[ReadonlyBlockProcessingControlFlags] = Encoder { (writer, flags) =>
  writer.writeInt(flags._flags)
}

given Decoder[ReadonlyBlockProcessingControlFlags] = Decoder { reader =>
  ReadonlyBlockProcessingControlFlags(reader.readInt())
}

