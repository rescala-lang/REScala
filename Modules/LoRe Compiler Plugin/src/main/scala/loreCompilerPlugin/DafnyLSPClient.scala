package loreCompilerPlugin

import DafnyLSPClient.constructLSPMessage
import os.{SubProcess, spawn}
import ujson.{Null, Obj, Str, Value, read as ujsonRead}
import upickle.default.{read as upickleRead, write as upickleWrite}

import java.nio.charset.StandardCharsets
import scala.collection.mutable.ArrayBuffer
import LSPDataTypes.*

class DafnyLSPClient {

  /** The subprocess running the language server. */
  private var process: Option[SubProcess] = None

  /** Initializes the internal instance running the Dafny language server, if not running.
    * If there is already a running instance present, this method does nothing.
    *
    * @param rootUri The root URI of the project the LS is being booted for.
    * @param initId The id to use for the LSP initialization message.
    */
  def initializeLSP(rootUri: String, initId: Int = 0): Unit = {
    if process.isDefined && process.get.isAlive() then {
      println("Attempted to initialize LSP when active process already exists")
      return
    }

    val lspProcess: SubProcess = spawn(cmd = ("dafny", "server"))
    process = Some(lspProcess)

    val initializeMessage: String = constructLSPMessage("initialize", Some(initId))(
      ("processId", Null),
      ("rootUri", Str(rootUri)),
      ("capabilities", Obj())
    )
    sendMessage(initializeMessage)
    val initRes: LSPResponse = waitForResponse(initId)

    if initRes.error.isDefined then {
      throw new Error(s"An error occurred initializing the LSP client:\n${upickleWrite(initRes.error.get)}")
    } else {
      println("Successfully initialized a connection with the language server.")
    }

    val initializedNotification: String = constructLSPMessage("initialized")()
    sendMessage(initializedNotification)
  }

  /** Shut down the internal instance running the Dafny language server, if running. */
  def shutdownLSP(): Unit = {
    if process.isDefined then {
      // Only try to destroy if it wasn't already
      if process.get.isAlive() then process.get.destroy()

      // Reset value regardless of whether it was alive or not
      process = None
    }
  }

  /** Sends a JSON-RPC message to the language server.
    *
    * The required message header is automatically prepended to the
    * input string and must not already be included in it.
    *
    * @param message The message to write.
    */
  def sendMessage(message: String): Unit = {
    val proc: SubProcess = process.getOrElse(throw new Error("LSP Client not initialized"))

    val msgBytes: Array[Byte] = message.getBytes(StandardCharsets.UTF_8)
    val msgHeader: String     = s"Content-Length: ${msgBytes.length}\r\n\r\n"

    println(s"The header to be written is:\n$msgHeader")
    println(s"The message to be written is:\n$message")

    println(s"Writing header to stdin...")
    proc.stdin.write(msgHeader)
    println("Writing message to stdin...")
    proc.stdin.write(message)
    println("Flushing input stream...")
    proc.stdin.flush()
  }

  /** Reads a message from the language server.
    *
    * @return The read message.
    */
  def readMessage(): LSPMessage = {
    val msgString: String = readStringMessage()

    parseLSPMessage(msgString)
  }

  /** Reads an JSON-RPC message from the language server by first reading the header until the two CRLF
    * sequences ("\r\n\r\n") are found, and then reading the JSON payload of the expected length.
    * This ignores any Content-Type headers, if included, and only processes Content-Length.
    * If there are no messages to be read, this will block until there is one.
    *
    * @return The read message.
    */
  private def readStringMessage(): String = {
    val proc: SubProcess = process.getOrElse(throw new Error("LSP Client not initialized"))

    println("Reading header from stdout...")
    val headerBytes: ArrayBuffer[Byte] = new ArrayBuffer[Byte]()
    val terminator: Array[Byte]        = "\r\n\r\n".getBytes(StandardCharsets.UTF_8)
    var foundTerminator: Boolean       = false

    // Find the header terminator
    while !foundTerminator do {
      val b: Byte = proc.stdout.readByte()
      headerBytes.append(b)

      if headerBytes.size >= terminator.length then {
        // Compare the last X bytes, where X is the terminator's byte length, to the terminator string
        // If the last X bytes equal the terminator, the end of the message header has been found
        val tail: Array[Byte] = headerBytes.takeRight(terminator.length).toArray

        if tail.sameElements(terminator) then {
          foundTerminator = true
        }
      }
    }

    val replyHeader: String = new String(headerBytes.toArray(), StandardCharsets.UTF_8)
    println(s"Header read:\n$replyHeader")

    println("Checking header for message length...")
    val contentLength: Int = "Content-Length:\\s*(\\d+)".r.findFirstMatchIn(replyHeader) match {
      case Some(m) => m.group(1).toInt
      case None    => throw new Exception("Content-Length header not found.")
    }

    println(s"Reading $contentLength bytes of message content...")
    val replyBytes: Array[Byte] = new Array[Byte](contentLength)
    proc.stdout.readFully(replyBytes)

    val replyMessage: String = new String(replyBytes, StandardCharsets.UTF_8)
    println(s"Read message content:\n$replyMessage")

    replyMessage
  }

  /** Parses a string containing a JSON message into a LSPMessage value. Used to read message from the server.
    *
    * @param message The JSON message to parse as a string.
    * @return The parsed JSON message as a LSPMessage value.
    */
  private def parseLSPMessage(message: String): LSPMessage = {
    val json: Value            = ujsonRead(message)
    var lspMessage: LSPMessage = null

    /*
      TODO: Checking for the error property is necessary because of a bug in the Dafny LS, in which it is
       missing the id field for a parse error / invalid request error entirely, rather than being "null".
       Once this has been fixed in the language server's responses, the check can be removed safely.
     */
    // Work around above bug by manually adding null id for errors when missing
    if json.obj.get("error").isDefined && json.obj.get("id").isEmpty then json("id") = Null

    /*
      LSPMessage is a sealed trait from which the different types of LSP case classes, i.e.
      requests, notifications and results, derive to provide a unified return type. However,
      for upickle to properly parse a json string into the adequate case class when using
      sealed traits, a "$type" property must be included in the json containing the name of
      the case class that parsing should result in. The LSP messages naturally do not include
      these. Therefore, find out the case class to use and manually add said property here.
      (The alternative to this solution would be to write a custom pickler for upickle instead.)
     */
    if json.obj.get("id").isEmpty then {
      // LSP Notifications

      if json("method").str == "dafny/textDocument/symbolStatus" then {
        json("$type") = "SymbolStatusNotification"
        lspMessage = upickleRead[SymbolStatusNotification](json)
      } else {
        // Any other notifications
        json("$type") = "LSPNotification"
        lspMessage = upickleRead[LSPNotification](json)
      }
    } else {
      // Could equivalently check for existence of the "method" key
      if json.obj.get("params").isDefined then {
        // LSP Request
        json("$type") = "LSPRequest"
        lspMessage = upickleRead[LSPRequest](json)
      } else {
        // LSP Response
        json("$type") = "LSPResponse"
        lspMessage = upickleRead[LSPResponse](json)
      }
    }

    // do more stuff with the parsed case class here
    lspMessage
  }

  /** Reads messages from the language server until a message with the "dafny/textDocument/symbolStatus"
    * method is found, in which the "status" parameter of all entries in the namedVerifiables list are either
    * "Error" or "Correct". Any messages read while waiting that don't match will be discarded.
    *
    * @return The symbol status notification.
    */
  def waitForVerificationResult(): (SymbolStatusNotification, Option[LSPNotification]) = {
    println(s"Waiting for a \"dafny/textDocument/symbolStatus\"-Notification of all status 4 or 5...")
    var latestReadMessage: LSPMessage                    = readMessage()
    var diagnosticsNotification: Option[LSPNotification] = None

    // Continue reading messages while the read message isn't a symbol status
    // notification, or any of the named verifiables are not on status 4/5 yet
    while !latestReadMessage.isInstanceOf[SymbolStatusNotification]
      || latestReadMessage.asInstanceOf[SymbolStatusNotification].params.namedVerifiables.exists(nv =>
        nv.status != VerificationStatus.Error && nv.status != VerificationStatus.Correct
      )
    do {
      // If a diagnostics message was read, check if it contains a fatal error (e.g. syntax) or just verification errors
      // If it contains a fatal error, throw an error. If it's just verification errors, save the message to return after.
      latestReadMessage match
        case diag: LSPNotification if diag.method == "textDocument/publishDiagnostics" =>
          // TODO: Check for fatal error vs verification error
          println("Diagnostics notification read while waiting for symbol status")
          diagnosticsNotification = Some(diag)
        case _ => ()

      latestReadMessage = readMessage()
    }

    println(s"symbolStatus notification with only status 4 or 5 found:\n${upickleWrite(latestReadMessage)}")

    (
      latestReadMessage.asInstanceOf[SymbolStatusNotification], // Cast is safe because of above check
      diagnosticsNotification
    )
  }

  /** Reads messages from the language server until a notification using the given method is found.
    * Any messages read while waiting that aren't notifications or don't match the given method will be discarded.
    *
    * @param method The method which is being waited for.
    * @return The notification using the given method.
    */
  def waitForNotification(method: String): LSPNotification = {
    println(s"Waiting for a notification with method $method...")
    var msg: LSPMessage = readMessage()

    while !msg.isInstanceOf[LSPNotification] || msg.asInstanceOf[LSPNotification].method != method
    do {
      msg = readMessage()
    }

    println(s"Message using method $method found:\n${upickleWrite(msg)}")
    msg.asInstanceOf[LSPNotification] // Cast is safe because of above check
  }

  /** Reads messages from the language server until a message using the given id is found.
    * Any messages read while waiting that don't match the given id will be discarded.
    *
    * @param id           The id which is being waited for.
    * @return The message using the given id.
    */
  def waitForResponse(id: Int): LSPResponse = {
    println(s"Waiting for a response with id $id...")
    var msg: LSPMessage = readMessage()

    while !msg.isInstanceOf[LSPResponse] || msg.asInstanceOf[LSPResponse].id != id
    do {
      msg = readMessage()
    }

    println(s"Response with id $id found:\n${upickleWrite(msg)}")
    msg.asInstanceOf[LSPResponse] // Cast is safe because of above check
  }

  /** Send the given message with the given id and wait for a response to it.
    *
    * @param message      The message to send.
    * @param id           The id of the message being sent.
    * @return The response to the message that was sent.
    */
  def sendAndWaitForResponse(
      message: String,
      id: Int
  ): LSPDataTypes.LSPResponse = {
    sendMessage(message)
    waitForResponse(id)
  }
}

object DafnyLSPClient {

  /** Creates a JSON string representation of the given LSP message parameters.
    * Used to build requests and notifications for the server.
    *
    * @param method The method of the message.
    * @param id     The id of the message. Required for requests, omitted for notifications.
    * @param params A list of message parameters. For primitive parameters such as a string or an integer,
    *               rather than a list of named parameters, specify said parameter's name to be "_primitive".
    * @return The string representing the message as a JSON object.
    */
  def constructLSPMessage(method: String, id: Option[Int] = None)(params: (String, Value)*): String = {
    val jsonObject: Value = Obj(
      "jsonrpc" -> "2.0",
      "method"  -> method
    )

    // Only add the "id" field if an id was supplied
    // Requests have ids, notifications don't
    if id.nonEmpty then jsonObject("id") = id.get

    params.length match
      case 0 => jsonObject("params") = Obj() // Use empty json object when no params are specified
      case 1 if params.head._1 == "_primitive" =>
        jsonObject("params") = params.head._2 // Primitive params (strings, etc)
      case _ => jsonObject("params") = params // Complex params are carried over as-is

    // Render ujson object to string
    jsonObject.toString
  }
}
