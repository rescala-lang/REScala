package dtn

import rdts.time.Dots

object BundleCreation {
  def createBundle(data: Array[Byte], full_destination_uri: String, full_source_uri: String): Bundle = {
    val primary_block: PrimaryBlock = PrimaryBlock(
      version = 7,
      bundle_processing_control_flags = BundleProcessingControlFlags(do_not_fragment = true),
      crc_type = 0,
      destination = Endpoint.createFrom(full_destination_uri),
      source = Endpoint.createFrom(full_source_uri),
      report_to = Endpoint.NONE_ENDPOINT,
      creation_timestamp = CreationTimestamp.NOW,
      lifetime = 1000 * 3600 * 24
    )

    val hop_count_block: HopCountBlock =
      HopCountBlock.createFrom(HopCount(hop_limit = 32, current_count = 0)).copy(block_number = 2)

    val payload_block: PayloadBlock = PayloadBlock.createFrom(data).copy(block_number = 1)

    Bundle(primary_block, List(hop_count_block, payload_block))
  }

  def createBundleUTF8(utf8_payload: String, full_destination_uri: String, full_source_uri: String): Bundle = {
    val primary_block: PrimaryBlock = PrimaryBlock(
      version = 7,
      bundle_processing_control_flags = BundleProcessingControlFlags(do_not_fragment = true),
      crc_type = 0,
      destination = Endpoint.createFrom(full_destination_uri),
      source = Endpoint.createFrom(full_source_uri),
      report_to = Endpoint.NONE_ENDPOINT,
      creation_timestamp = CreationTimestamp.NOW,
      lifetime = 1000 * 3600 * 24
    )

    val hop_count_block: HopCountBlock =
      HopCountBlock.createFrom(HopCount(hop_limit = 32, current_count = 0)).copy(block_number = 2)

    val payload_block: PayloadBlock = PayloadBlock.createFrom(utf8_payload).copy(block_number = 1)

    Bundle(primary_block, List(hop_count_block, payload_block))
  }

  def createBundleRdt(
      data: Array[Byte],
      dots: Dots,
      node: Endpoint,
      full_destination_uri: String,
      full_source_uri: String
  ): Bundle = {
    val primary_block: PrimaryBlock = PrimaryBlock(
      version = 7,
      bundle_processing_control_flags = BundleProcessingControlFlags(do_not_fragment = true),
      crc_type = 0,
      destination = Endpoint.createFrom(full_destination_uri),
      source = Endpoint.createFrom(full_source_uri),
      report_to = Endpoint.NONE_ENDPOINT,
      creation_timestamp = CreationTimestamp.NOW,
      lifetime = 1000 * 3600 * 24
    )

    val rdtmeta_block: RdtMetaBlock = RdtMetaBlock.createFrom(dots).copy(block_number = 4)

    val hop_count_block: HopCountBlock =
      HopCountBlock.createFrom(HopCount(hop_limit = 32, current_count = 0)).copy(block_number = 3)

    val previous_node_block: PreviousNodeBlock = PreviousNodeBlock.createFrom(node).copy(block_number = 2)

    val payload_block: PayloadBlock = PayloadBlock.createFrom(data).copy(block_number = 1)

    Bundle(primary_block, List(rdtmeta_block, hop_count_block, previous_node_block, payload_block))
  }
}
