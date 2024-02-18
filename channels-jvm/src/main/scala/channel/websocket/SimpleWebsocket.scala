package channel.websocket

import java.io.{DataInputStream, IOException, InputStream, OutputStream}
import java.net.{ServerSocket, Socket}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, NoSuchAlgorithmException}
import java.util.{Base64, Scanner}
import java.util.regex.{Matcher, Pattern}
import scala.annotation.tailrec;

object SimpleWebsocket {
  def main(args: Array[String]): Unit = {
    val server = new ServerSocket(8080);
    System.out.println("Server has started on :80.\r\nWaiting for a connection…");
    val client = server.accept();
    System.out.println("A client connected.")

    val in  = client.getInputStream
    val out = client.getOutputStream
    val s   = new Scanner(in, StandardCharsets.UTF_8)

    val data = s.useDelimiter("\\r\\n\\r\\n").next
    val get  = Pattern.compile("^GET").matcher(data)

    if (get.find()) {
      val `match` = Pattern.compile("Sec-WebSocket-Key: (.*)").matcher(data)
      `match`.find()
      println(`match`.group(1))
      val response =
        ("HTTP/1.1 101 Switching Protocols\r\n" +
          "Connection: Upgrade\r\n" +
          "Upgrade: websocket\r\n" +
          "Sec-WebSocket-Accept: " +
          Base64.getEncoder.encodeToString(
            MessageDigest.getInstance("SHA-1").digest(
              (`match`.group(1) + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").getBytes("UTF-8")
            )
          ) + "\r\n\r\n").getBytes(StandardCharsets.UTF_8)
      out.write(response, 0, response.length)
    }

    def readByte() =
      val res = in.read()
      if res < 0 then throw IllegalStateException(s"socket closed")
      res.toByte

    while
      val firstByte = readByte()
      val fin       = (firstByte & 128) != 0
      val opcode    = firstByte & 15

      val secondByte = readByte()
      val mask       = (secondByte & 128) != 0
      val len        = (secondByte & 127)

      val length: Long = len match
        case 126 =>
          val upper = readByte()
          val lower = readByte()
          (upper.toInt << 16) | (lower.toInt)
        case 127 =>
          ByteBuffer.wrap(in.readNBytes(8)).getLong
        case other => other

      val maskKey =
        if mask
        then in.readNBytes(4)
        else Array[Byte](0, 0, 0, 0)

      if length > Int.MaxValue then throw IllegalStateException("message too large")

      val payload =
        val encoded = in.readNBytes(length.toInt)
        def rec(offset: Int): Unit =
          if offset >= encoded.length then return
          encoded.update(offset, (encoded(offset) ^ maskKey(offset & 3)).toByte)
          rec(offset + 1)
        rec(0)
        encoded

//      import jdk.incubator.vector.{ByteVector, VectorOperators}
//      val decoded = {
//        val species = ByteVector.SPECIES_PREFERRED
//
//        val expandedMask = Iterator.continually(maskKey).flatten.take(species.length() + 4).toArray
//
//        val decoded = new Array[Byte](encoded.length)
//
//        @tailrec
//        def rec(i: Int): Unit = {
//
//          if i >= encoded.length then return
//
//          val m = species.indexInRange(i, encoded.length)
//
//          val offset  = 0
//          val encVec  = ByteVector.fromArray(species, encoded, offset, m)
//          val maskVec = ByteVector.fromArray(species, expandedMask, offset % 4, m)
//
//          val res = encVec.lanewise(VectorOperators.XOR, maskVec)
//
//          res.intoArray(decoded, i, m)
//
//          rec(i + species.length())
//        }
//
//        rec(0)
//
//        decoded
//      }

      val res = opcode match
        case 0x2 =>
          new String(payload, StandardCharsets.UTF_8)

      println(s"fin: $fin; opcode: $opcode; mask: ${maskKey.toList}; len: $length; $res")

      false
    do ()

  }
}
