package dtn

import io.bullet.borer.{AdtEncodingStrategy, Decoder, Encoder, Codec}
import io.bullet.borer.derivation.MapBasedCodecs.*

import io.bullet.borer.NullOptions.given  // dtn7-rs erouting encodes rust None-Options as null

given AdtEncodingStrategy = AdtEncodingStrategy.flat(typeMemberName = "type")  // dtn7-rs erouting encodes packet-enum flat with type key


/* This module supports:
serialization: object -> JSON
deserialization: JSON -> object
*/


// example encoded: {"Ip":"192.168.217.128"}  // idk about the others, just guessing in decoder/encoder
enum PeerAddress:
  case Ip(address: String)
  case BroadcastGeneric(domain: String, address: String)
  case Generic(address: String)

// example encoded: "DispatchPending"
enum Constraint derives Codec:
  case DispatchPending
  case ForwardPending
  case ReassemblyPending
  case Contraindicated
  case LocalEndpoint
  case Deleted

// example encoded: "Static"
enum PeerType derives Codec:
  case Static
  case Dynamic

// example encoded: {"eid": ..., "addr": ...}  // no class type info
case class DtnPeer(eid: Endpoint, addr: PeerAddress, con_type: PeerType, period: Option[Int], cla_list: List[Tuple2[String, Option[Int]]], services: Map[Int, String], last_contact: Int, fails: Int) derives Codec

// example encoded: {"source": ..., "destination": ...}  // no class type info
case class BundlePack(source: Endpoint, destination: Endpoint, received_time: Long, creation_time: Long, lifetime: Long, id: String, administrative: Boolean, size: Int, constraints: List[Constraint]) derives Codec

// example encoded: {"remote": ..., "port": ...}  // no class type info
case class Sender(remote: PeerAddress, port: Option[Int], agent: String, next_hop: Endpoint) derives Codec


// example encoded: {"type": "Error", "reason": "some error reason"}
enum Packet derives Codec.All:
  // packets sent from dtnd
  case RequestSenderForBundle(clas: List[String], bp: BundlePack)
  case Error(reason: String)
  case Timeout(bp: BundlePack)
  case SendingFailed(bid: String, cla_sender: String)
  case SendingSucceeded(bid: String, cla_sender: String)
  case IncomingBundle(bndl: Bundle)
  case IncomingBundleWithoutPreviousNode(bid: String, node_name: String)
  case EncounteredPeer(name: String, eid: Endpoint, peer: DtnPeer)
  case DroppedPeer(name: String, eid: Endpoint)
  case PeerState(peers: Map[String, DtnPeer])
  case ServiceState(service_list: Map[Int, String])
  // packets sent from client
  case AddService(tag: Int, service: String)
  case ResponseSenderForBundle(bp: BundlePack, clas: List[Sender], delete_afterwards: Boolean)



given Encoder[PeerAddress] = Encoder { (writer, peerAddress) =>
  writer.writeMapOpen(1)

  peerAddress match
    case PeerAddress.Ip(address) => writer.writeMapMember("Ip", address)
    case PeerAddress.BroadcastGeneric(domain, address) => writer.writeMapMember("BroadcastGeneric", (domain, address))
    case PeerAddress.Generic(addr) => writer.writeMapMember("Generic", addr)
  
  writer.writeMapClose()
}

given Decoder[PeerAddress] = Decoder { reader =>
  val unbounded = reader.readMapOpen(1)

  var peerAddress: PeerAddress = null;

  reader.readString() match
    case "Ip" => peerAddress = PeerAddress.Ip(reader.readString())
    case "BroadcastGeneric" => {
      val arr_unbounded = reader.readArrayOpen(2)
      peerAddress = PeerAddress.BroadcastGeneric(reader.readString(), reader.readString())
      reader.readArrayClose(arr_unbounded, peerAddress)
    }
    case "Generic" => peerAddress = PeerAddress.Generic(reader.readString())
    case s: Any => throw Exception(s"unknown PeerAddress type: $s")
  
  reader.readMapClose(unbounded, peerAddress)
}
