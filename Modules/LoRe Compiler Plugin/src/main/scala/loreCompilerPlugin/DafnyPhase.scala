package loreCompilerPlugin

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.transform.Inlining
import loreCompilerPlugin.lsp.DafnyLSPClient
import loreCompilerPlugin.lsp.LSPDataTypes.*
import scala.util.matching.Regex
import ujson.Obj

object DafnyPhase {
  val name: String        = "Dafny"
  val description: String = "generates Dafny source code from previously constructed LoRe AST nodes"
}

class DafnyPhase extends PluginPhase {
  val phaseName: String                = DafnyPhase.name
  override val description: String     = DafnyPhase.description
  override val runsAfter: Set[String]  = Set(LoRePhase.name)
  override val runsBefore: Set[String] = Set(Inlining.name)

  println("dafny phase initialized")

  // Run phase once for all compilation units ("run" is ran for each compilation unit individually)
  override def runOn(units: List[CompilationUnit])(using ctx: Context): List[CompilationUnit] = {
    // First, run the default runOn method for regular Scala compilation (do not remove this).
    val result = super.runOn(units)

    println("dafny phase running")

    // Only need to initialize the LSP client once
    val lspClient: DafnyLSPClient = new DafnyLSPClient()
//    val folderPath: String = units.head.source.path.substring(0, units.head.source.path.indexOf(units.head.source.name))
    val folderPath: String = "file:///D:/Repositories/thesis-code/dafny"
    lspClient.initializeLSP(folderPath)
    var counter: Int = 0

    // Then, run the unit-specific code for each unit individually
    for unit <- units do {
      counter += 1
//      val filePath: String = unit.source.path.replace(".scala", ".dfy")
      val filePath: String = s"file:///D:/Repositories/thesis-code/dafny/test${counter}.dfy"
      // todo: this is dummy code, normally output by the to-be-implemented dafny generator
      val dafnyCode: String =
        s"""method Test(x: int) returns (y: int)
          |  ensures {:error "Error on LoRe ln X, col Y"} y == ${if counter == 1 then "x" else counter}
          |  {
          |    y := x;
          |  }
          |
          |method Main()
          |{
          |  var a: int := Test(0);
          |  print a;
          |}""".stripMargin

      val didOpenMessage: String = DafnyLSPClient.constructLSPMessage("textDocument/didOpen")(
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

      val (verificationResult: SymbolStatusNotification, diagnosticsNotification: Option[LSPNotification]) =
        lspClient.waitForVerificationResult()

      val erroneousVerifiables: List[NamedVerifiable] =
        verificationResult.params.namedVerifiables.filter(nv => nv.status == VerificationStatus.Error)

      if erroneousVerifiables.isEmpty then {
        println("No unverifiable claims could be found in the program.")
      } else {
        println("Some claims in the program could not be verified.")
      }
    }

    // Always return result of default runOn method for regular Scala compilation, as we do not modify it.
    result
  }
}
