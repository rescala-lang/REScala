package dtn

object BundleCreation {
  def createBundle(data: Array[Byte], full_destination_uri: String, full_source_url: String, lifetime: Int = 3600 * 24 * 1000): Bundle = {
    val primary_block: PrimaryBlock = PrimaryBlock(
      version = 7,
      bundle_processing_control_flags = BundleProcessingControlFlags(do_not_fragment = true),
      crc_type = 0,
      destination = Endpoint.createFrom(full_destination_uri),
      source = Endpoint.createFrom(full_source_url),
      report_to = Endpoint.NONE_ENDPOINT,
      creation_timestamp = CreationTimestamp.NOW,
      lifetime = 1000 * 3600 * 24
    )

    val hop_count_block: HopCountBlock = HopCountBlock.createFrom(HopCount(hop_limit = 32, current_count = 0)).copy(block_number = 2)

    val payload_block: PayloadBlock = PayloadBlock.createFrom(data).copy(block_number = 1)

    Bundle(primary_block, List(hop_count_block, payload_block))
  }


  def createBundleUTF8(utf8_payload: String, full_destination_uri: String, full_source_url: String, lifetime: Int = 3600 * 24 * 1000): Bundle = {
    val primary_block: PrimaryBlock = PrimaryBlock(
      version = 7,
      bundle_processing_control_flags = BundleProcessingControlFlags(do_not_fragment = true),
      crc_type = 0,
      destination = Endpoint.createFrom(full_destination_uri),
      source = Endpoint.createFrom(full_source_url),
      report_to = Endpoint.NONE_ENDPOINT,
      creation_timestamp = CreationTimestamp.NOW,
      lifetime = 1000 * 3600 * 24
    )

    val hop_count_block: HopCountBlock = HopCountBlock.createFrom(HopCount(hop_limit = 32, current_count = 0)).copy(block_number = 2)

    val payload_block: PayloadBlock = PayloadBlock.createFrom(utf8_payload).copy(block_number = 1)

    Bundle(primary_block, List(hop_count_block, payload_block))
  }
}
