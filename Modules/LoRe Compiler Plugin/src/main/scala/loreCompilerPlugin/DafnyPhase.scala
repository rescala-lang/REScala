package loreCompilerPlugin

import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.transform.Inlining
import ujson.Obj
import loreCompilerPlugin.LSPDataTypes.{LSPNotification, NamedVerifiable, SymbolStatusNotification, VerificationStatus}

object DafnyPhase {
  val name: String        = "Dafny"
  val description: String = "generates Dafny source code from previously constructed LoRe AST nodes"
}

class DafnyPhase extends PluginPhase {
  val phaseName: String                = DafnyPhase.name
  override val description: String     = DafnyPhase.description
  override val runsAfter: Set[String]  = Set(LoRePhase.name)
  override val runsBefore: Set[String] = Set(Inlining.name)

  println(s"entered $phaseName compiler phase")

  // TODO: Begin of test embedding for lsp code
  private val folderPath: String = "file:///D:/Repositories/thesis-code/dafny"
  private val filePath: String   = "file:///D:/Repositories/thesis-code/dafny/test.dfy"
  private val dafnyCode: String =
    """method Test(x: int) returns (y: int)
      |  ensures {:error "Error on LoRe ln X, col Y"} y == 0
      |  {
      |    y := x;
      |  }
      |
      |method Main()
      |{
      |  var a: int := Test(0);
      |  print a;
      |}""".stripMargin

  private val lspClient: DafnyLSPClient = new DafnyLSPClient()
  lspClient.initializeLSP(folderPath)

  private val didOpenMessage: String = DafnyLSPClient.constructLSPMessage("textDocument/didOpen")(
    (
      "textDocument",
      Obj(
        "uri"        -> filePath,
        "languageId" -> "dafny",
        "version"    -> 1,
        "text"       -> dafnyCode
      )
    ),
  )
  lspClient.sendMessage(didOpenMessage)

  private val (verificationResult: SymbolStatusNotification, diagnosticsNotification: Option[LSPNotification]) =
    lspClient.waitForVerificationResult()

  private val erroneousVerifiables: List[NamedVerifiable] =
    verificationResult.params.namedVerifiables.filter(nv => nv.status == VerificationStatus.Error)

  if erroneousVerifiables.isEmpty then {
    println("No unverifiable claims could be found in the program.")
  } else {
    println("Some claims in the program could not be verified.")
  }
  // TODO: End test embedding of lsp code
}
