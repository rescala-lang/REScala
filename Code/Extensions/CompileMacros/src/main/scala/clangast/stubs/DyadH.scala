package clangast.stubs

import clangast.decl.{CEnumConstantDecl, CFunctionDecl, CInclude}
import clangast.types.{CType, CTypedefType}
import compiler.context.TranslationContext

object DyadH extends CLibraryStub {
  override val include: CInclude = CInclude("dyad.h", true)

  def dyad_Socket(using TranslationContext): CType = includeStub(CTypedefType("dyad_Socket"))
  def dyad_Stream(using TranslationContext): CType = includeStub(CTypedefType("dyad_Stream"))
  def dyad_Event(using TranslationContext): CType  = includeStub(CTypedefType("dyad_Event"))

  val typeField   = "type"
  val udataField  = "udata"
  val streamField = "stream"
  val remoteField = "remote"
  val msgField    = "msg"
  val dataField   = "data"
  val sizeField   = "size"

  def DYAD_EVENT_NULL(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_EVENT_NULL", None))
  def DYAD_EVENT_DESTROY(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_EVENT_DESTROY", None))
  def DYAD_EVENT_ACCEPT(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_EVENT_ACCEPT", None))
  def DYAD_EVENT_LISTEN(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_EVENT_LISTEN", None))
  def DYAD_EVENT_CONNECT(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_EVENT_CONNECT", None))
  def DYAD_EVENT_CLOSE(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_EVENT_CLOSE", None))
  def DYAD_EVENT_READY(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_EVENT_READY", None))
  def DYAD_EVENT_DATA(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_EVENT_DATA", None))
  def DYAD_EVENT_LINE(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_EVENT_LINE", None))
  def DYAD_EVENT_ERROR(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_EVENT_ERROR", None))
  def DYAD_EVENT_TIMEOUT(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_EVENT_TIMEOUT", None))
  def DYAD_EVENT_TICK(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_EVENT_TICK", None))

  def DYAD_STATE_CLOSED(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_STATE_CLOSED", None))
  def DYAD_STATE_CLOSING(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_STATE_CLOSING", None))
  def DYAD_STATE_CONNECTING(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_STATE_CONNECTING", None))
  def DYAD_STATE_CONNECTED(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_STATE_CONNECTED", None))
  def DYAD_STATE_LISTENING(using TranslationContext): CEnumConstantDecl =
    includeStub(CEnumConstantDecl("DYAD_STATE_LISTENING", None))

  def dyad_init(using TranslationContext): CFunctionDecl            = includeStub(CFunctionStub("dyad_init"))
  def dyad_update(using TranslationContext): CFunctionDecl          = includeStub(CFunctionStub("dyad_update"))
  def dyad_shutdown(using TranslationContext): CFunctionDecl        = includeStub(CFunctionStub("dyad_shutdown"))
  def dyad_getVersion(using TranslationContext): CFunctionDecl      = includeStub(CFunctionStub("dyad_getVersion"))
  def dyad_getTime(using TranslationContext): CFunctionDecl         = includeStub(CFunctionStub("dyad_getTime"))
  def dyad_getStreamCount(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("dyad_getStreamCount"))
  def dyad_setTickInterval(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("dyad_setTickInterval"))
  def dyad_setUpdateTimeout(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("dyad_setUpdateTimeout"))
  def dyad_atPanic(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("dyad_atPanic"))

  def dyad_newStream(using TranslationContext): CFunctionDecl      = includeStub(CFunctionStub("dyad_newStream"))
  def dyad_listen(using TranslationContext): CFunctionDecl         = includeStub(CFunctionStub("dyad_listen"))
  def dyad_listenEx(using TranslationContext): CFunctionDecl       = includeStub(CFunctionStub("dyad_listenEx"))
  def dyad_connect(using TranslationContext): CFunctionDecl        = includeStub(CFunctionStub("dyad_connect"))
  def dyad_addListener(using TranslationContext): CFunctionDecl    = includeStub(CFunctionStub("dyad_addListener"))
  def dyad_removeListener(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("dyad_removeListener"))
  def dyad_removeAllListeners(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("dyad_removeAllListeners"))
  def dyad_end(using TranslationContext): CFunctionDecl          = includeStub(CFunctionStub("dyad_end"))
  def dyad_close(using TranslationContext): CFunctionDecl        = includeStub(CFunctionStub("dyad_close"))
  def dyad_write(using TranslationContext): CFunctionDecl        = includeStub(CFunctionStub("dyad_write"))
  def dyad_vwritef(using TranslationContext): CFunctionDecl      = includeStub(CFunctionStub("dyad_vwritef"))
  def dyad_writef(using TranslationContext): CFunctionDecl       = includeStub(CFunctionStub("dyad_writef"))
  def dyad_setTimeout(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("dyad_setTimeout"))
  def dyad_setNoDelay(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("dyad_setNoDelay"))
  def dyad_getState(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("dyad_getState"))
  def dyad_getAddress(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("dyad_getAddress"))
  def dyad_getPort(using TranslationContext): CFunctionDecl      = includeStub(CFunctionStub("dyad_getPort"))
  def dyad_getBytesSent(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("dyad_getBytesSent"))
  def dyad_getBytesReceived(using TranslationContext): CFunctionDecl =
    includeStub(CFunctionStub("dyad_getBytesReceived"))
  def dyad_getSocket(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("dyad_getSocket"))
}
