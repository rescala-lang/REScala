package dtn2

import io.bullet.borer.{Decoder, Encoder}

import scala.collection.mutable.ListBuffer


case class Endpoint(scheme: Int, specific_part: String | Int)

case class CreationTimestamp(bundle_creation_time: Long, sequence_number: Int)

case class FragmentInfo(fragment_offset: Int, total_application_data_length: Int)

case class PrimaryBlock(version: Int,
                        bundle_processing_control_flags: Int,
                        crc_type: Int,
                        destination: Endpoint,
                        source: Endpoint,
                        report_to: Endpoint,
                        creation_timestamp: CreationTimestamp,
                        lifetime: Int = 1000 * 3600 * 24,
                        var fragment_info: Option[FragmentInfo] = None,
                        var crc: Option[Array[Byte]] = None)

case class CanonicalBlock(block_type_code: Int,
                          block_number: Int,
                          block_processing_control_flags: Int,
                          crc_type: Int,
                          data: Array[Byte],
                          var crc: Option[Array[Byte]] = None)


case class Bundle(primary_block: PrimaryBlock, other_blocks: List[CanonicalBlock])



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



given Encoder[PrimaryBlock] = Encoder { (writer, primaryBlock) => 
  // optional empty fields are not encoded, so we need to calculate our length beforehand
  var length = 8
  if (primaryBlock.fragment_info.isDefined) length += 1
  if (primaryBlock.crc.isDefined) length += 1

  writer
    .writeArrayOpen(length)
    .writeInt(primaryBlock.version)
    .writeInt(primaryBlock.bundle_processing_control_flags)
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
    bundle_processing_control_flags = reader.readInt(), 
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
    .writeInt(block.block_processing_control_flags)
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

  val block = CanonicalBlock(
    block_type_code = reader.readInt(),
    block_number = reader.readInt(),
    block_processing_control_flags = reader.readInt(),
    crc_type = reader.readInt(),
    data = reader.readByteArray()
  )

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

