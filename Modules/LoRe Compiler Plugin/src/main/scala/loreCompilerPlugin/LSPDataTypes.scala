package loreCompilerPlugin

import upickle.default.ReadWriter
import ujson.Value

/** Various Data structures for parsing any kind of message sent from the language server to the client.
  * Not intended be used for constructing messages to send to the Dafny Language Server, such usage may break.
  */
object LSPDataTypes {
  /* ------------------- Sealed traits for case classes --------------------- */

  // All JSON-RPC messages contain must the "jsonrpc" property, in this case version 2.0 by default
  sealed trait LSPMessage derives ReadWriter {
    val jsonrpc: String = "2.0"
  }

  /* ------------------- Generic message case classes --------------------- */

  case class LSPRequest(
      method: String,
      // Params can have different forms, so store them generically on initial deserialization
      params: Option[Value] = None,
      id: Int
  ) extends LSPMessage

  case class LSPNotification(
      method: String,
      // Params can have different forms, so store them generically on initial deserialization
      params: Option[Value] = None
  ) extends LSPMessage

  /** LSP Responses may contain a result _or_ an error, but never both at the same time. */
  case class LSPResponse(
      // Result can have different forms, so store it generically on initial deserialization
      result: Option[Value] = None,
      error: Option[ResponseError] = None,
      id: Int
  ) extends LSPMessage

  case class ResponseError(
      code: ErrorCode,
      message: String,
      // Error data can have varying structure, so store it generically on initial deserialization
      data: Option[Value] = None
  ) derives ReadWriter

  /** An enum of errors that can be returned in responses from the language server.
    * @param code The integer code of this error.
    */
  enum ErrorCode(val code: Int) {
    case ParseError     extends ErrorCode(-32700)
    case InvalidRequest extends ErrorCode(-32600)
    case MethodNotFound extends ErrorCode(-32601)
    case InvalidParams  extends ErrorCode(-32602)
    case InternalError  extends ErrorCode(-32603)
    // Further cases are implementation-specific
  }

  object ErrorCode {
    // Custom pickler for upickle so we can deserialize integers into error codes
    implicit val rw: ReadWriter[ErrorCode] = upickle.default.readwriter[Int].bimap[ErrorCode](
      // Serialization: From ErrorCode enum to integer
      error => error.code,
      // Deserialization: From integer to ErrorCode enum
      {
        case -32700 => ErrorCode.ParseError
        case -32600 => ErrorCode.InvalidRequest
        case -32601 => ErrorCode.MethodNotFound
        case -32602 => ErrorCode.InvalidParams
        case -32603 => ErrorCode.InternalError
        // Can't have a catch-all "Unknown Error" enum case since that loses the int value
        case i => throw new Error(s"Unknown LSP error code: $i")
      }
    )
  }

  case class LSPRange(
      start: LSPPosition,
      end: LSPPosition
  ) derives ReadWriter

  case class LSPPosition(
      line: Int,
      character: Int
  ) derives ReadWriter

  /* ------------------- Method-specific case classes --------------------- */

  case class SymbolStatusNotification(
      method: String = "dafny/textDocument/symbolStatus",
      params: SymbolStatusParams
  ) extends LSPMessage

  case class SymbolStatusParams(
      uri: String,
      version: Int,
      namedVerifiables: List[NamedVerifiable]
  ) derives ReadWriter

  case class NamedVerifiable(
      nameRange: LSPRange,
      status: VerificationStatus
  ) derives ReadWriter

  enum VerificationStatus(val status: Int) {
    case Stale   extends VerificationStatus(0)
    case Queued  extends VerificationStatus(1)
    case Running extends VerificationStatus(2)
    // Status 3 does not exist at time of writing: https://github.com/dafny-lang/dafny/blob/2473d8da29c2b934b5465ef92073305f34910596/Source/DafnyLanguageServer/Workspace/FileVerificationStatus.cs#L66
    case Error   extends VerificationStatus(4)
    case Correct extends VerificationStatus(5)
  }

  object VerificationStatus {
    // Custom pickler for upickle so we can deserialize integers into verification statuses
    implicit val rw: ReadWriter[VerificationStatus] = upickle.default.readwriter[Int].bimap[VerificationStatus](
      // Serialization: From VerificationStatus enum to integer
      error => error.status,
      // Deserialization: From integer to VerificationStatus enum
      {
        case 0 => VerificationStatus.Stale
        case 1 => VerificationStatus.Queued
        case 2 => VerificationStatus.Running
        case 4 => VerificationStatus.Error
        case 5 => VerificationStatus.Correct
        case i => throw new Error(s"Unknown Dafny verification status: $i")
      }
    )
  }
}
