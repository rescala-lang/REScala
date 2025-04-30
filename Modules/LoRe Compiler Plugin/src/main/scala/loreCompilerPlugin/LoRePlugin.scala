package loreCompilerPlugin

import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.ast.Trees.{Block, DefDef, Tree, Literal}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.{Inlining, Pickler}
import dotty.tools.dotc.util.SourceFile
import java.io.File // For getting URIs and the system-independent path separator
import lore.ast.Term
import loreCompilerPlugin.annotation.LoReProgram
import loreCompilerPlugin.codegen.LoReGen.*
import loreCompilerPlugin.codegen.DafnyGen.generate as generateDafnyCode
import loreCompilerPlugin.lsp.DafnyLSPClient
import loreCompilerPlugin.lsp.LSPDataTypes.{LSPNotification, NamedVerifiable, SymbolStatusNotification, VerificationStatus}
import ujson.Obj

import scala.annotation.nowarn

class LoRePlugin extends StandardPlugin {
  val name: String        = "LoRe Compiler Plugin"
  val description: String = "Verifies Scala-embedded LoRe code through Dafny"

  @nowarn // which variant to override depends on the scala version, use the old one until 3.5 is more stable
  override def init(options: List[String]): List[PluginPhase] = List(new LoRePhase)
}

object LoRePhase {
  val name: String        = "LoRe"
  val description: String = "verifies LoRe source code through compilation to Dafny"
}

class LoRePhase extends PluginPhase {
  val phaseName: String                = LoRePhase.name
  override val description: String     = LoRePhase.description
  override val runsAfter: Set[String]  = Set(Pickler.name)
  override val runsBefore: Set[String] = Set(Inlining.name)

  private var loreTerms: Map[(SourceFile, Symbol), List[Term]] = Map()

  override def transformDefDef(tree: tpd.DefDef)(using ctx: Context): tpd.Tree = {
    // Only process DefDefs marked as LoreProgram
    val loreAnnotation = tree.symbol.annotations.find(annot => annot.symbol.name.toString == "LoReProgram")

    if loreAnnotation.isDefined then {
      val programContents: List[Tree[?]] =
        tree.rhs match
          case block: Block[?] =>
            // All of the block's trees, apart from the very last (which is used as return value), are included
            // in the "stats" property. We also want the last one however, so add it to the list manually.
            // The exception to this is when the last tree is a definition, where expr is an Constant Literal of Unit.
            block.expr match
              case Literal(Constant(_: Unit)) => block.stats
              case _                          => block.stats :+ block.expr
          case _ => List()

      // Process each individual part of this LoRe program
      for tree <- programContents do {
        // Generate LoRe term for this tree
        val loreTerm: List[Term] = createLoreTermFromTree(tree)

        // Add LoRe term to the respective program's term list, or create a term list if it doesn't exist yet
        val programTermList: Option[List[Term]] = loreTerms.get((tree.source, ctx.owner))
        programTermList match
          case Some(list) =>
            val newList: List[Term] = list :++ loreTerm // Append list with loreTerm list
            loreTerms = loreTerms.updated((tree.source, ctx.owner), newList)
          case None =>
            println(s"Adding new term list to Map for ${ctx.owner.toString} in ${tree.source.name}")
            val newList: List[Term] = loreTerm // loreTerm is already a list
            loreTerms = loreTerms.updated((tree.source, ctx.owner), newList)
      }
    }

    // Original Scala tree is returned for regular Scala compilation to proceed
    tree
  }

  override def runOn(units: List[CompilationUnit])(using ctx: Context): List[CompilationUnit] = {
    // First, run the runOn method for regular Scala compilation (do not remove this or this phase breaks).
    // This will cause the compiler to call above-defined methods which will generate the LoRe AST nodes.
    val result = super.runOn(units)

    println("LoRe AST node generation was run for all compilation units.")
    println(s"Processed ${loreTerms.size} compilation units with ${loreTerms.map(_._2.size).mkString(",")} terms each.")

    // Initialize LSP client that will be used for verification after codegen
    val lspClient: DafnyLSPClient = new DafnyLSPClient()

    // Get the root path of the project these compilation units are from.
    // Basically, cut off anything that comes after "src/main/scala".
    // Have to do some messing around with escaping slashes and URIs here.
    val unitPath: String    = units.head.source.path
    val rootPattern: String = List("src", "main", "scala").mkString(File.separator) // Windows/Unix separators
    val rootPatternEscaped: String = rootPattern.replace("\\", "\\\\") // Gotta escape since split takes regex
    // Take the first half of the split, then add the split off separator back on and get the path as an URI string
    val folderPath: String = File(unitPath.split(rootPatternEscaped).head.concat(rootPattern)).toURI.toString
    lspClient.initializeLSP(folderPath)

    var counter: Int = 0

    // Iterate through all term lists and generate Dafny code for them + verify it
    for termList <- loreTerms do {
      println(s"Now processing LoRe AST parts for compilation unit: ${termList._1}.")

      // Turn the filepath into an URI and then sneakily change the file extension the LSP gets to see
      val filePath: String = File(termList._1._1.path).toURI.toString.replace(".scala", ".dfy")

      // Generate Dafny code from term list
      val dafnyRes: String = generateDafnyCode(termList._2, termList._1._2.name.toString)

      counter += 1
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

      // Send the generated code "file" to the language server to verify
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

      // Wait for verification results and then filter out any verification errors that occurred
      val (verificationResult: SymbolStatusNotification, diagnosticsNotification: Option[LSPNotification]) =
        lspClient.waitForVerificationResult()

      val erroneousVerifiables: List[NamedVerifiable] =
        verificationResult.params.namedVerifiables.filter(nv => nv.status == VerificationStatus.Error)

      // Process potential verification errors
      if erroneousVerifiables.isEmpty then {
        println("No unverifiable claims could be found in the program.")
        // This report is a debug log, remove this later and make it silent
        report.log("All claims were verified successfully.")
      } else {
        println("Some claims in the program could not be verified.")
        // Add details to this later, just simple debug for now
        report.error("Some claims could not be verified.")
      }
    }

    // Always return result of default runOn method for regular Scala compilation, as we do not modify it.
    result
  }
}
