package api2

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.{CAssignmentExpr, CGreaterThanExpr, CLessThanExpr, COrExpr}
import clangast.expr.unaryop.CNotExpr
import clangast.stmt.*
import clangast.stubs.{CJSONH, DyadH, StdBoolH, StdLibH}
import clangast.types.*
import compiler.CompilerCascade
import compiler.context.TranslationContext
import compiler.base.*
import compiler.base.CompileDataStructure.{deepCopy, release, retain}
import compiler.ext.*
import compiler.ext.CompileSerialization.{serialize, deserialize}

import java.io.{File, FileWriter}
import scala.collection.mutable.ArrayBuffer
import scala.quoted.*

class GraphCompiler(using Quotes)(
  reactives: List[CompiledReactive],
  outputReactives: List[CompiledReactive],
  appName: String
)(using ctx: TranslationContext, cascade: CompilerCascade) {
  import quotes.reflect.*

  private val sources: List[CompiledReactive] = reactives.filter(_.isSource)
  sources.foreach(s => cascade.dispatchLifted(_.compileDeserialize)(TypeRepr.of[Option].appliedTo(s.typeRepr)))
  private val signals: List[CompiledSignal] = reactives.collect { case s: CompiledSignal => s }
  private val events: List[CompiledEvent] = reactives.collect { case e: CompiledEvent => e }

  private val nameToReactive: Map[String, CompiledReactive] = reactives.map(r => r.name -> r).toMap
  private val reactiveInputs: Map[CompiledReactive, List[CompiledReactive]] =
    reactives.map(r => r -> r.inputs.map(nameToReactive)).toMap
  private val dataflow: Map[CompiledReactive, Set[CompiledReactive]] =
    reactives.foldLeft(Map.empty[CompiledReactive, Set[CompiledReactive]]) { (acc, r) =>
      addValueToAllKeys(acc, reactiveInputs(r), r)
    }

  private val topological: List[CompiledReactive] = toposort(sources).reverse
  private val sourcesTopological: List[CompiledReactive] = topological.filter(_.isSource)

  private def addValueToAllKeys[K, V](originalMap: Map[K, Set[V]], keys: List[K], value: V): Map[K, Set[V]] = {
    keys.foldLeft(originalMap) { (acc, i) =>
      acc.updatedWith(i) {
        case None => Some(Set(value))
        case Some(s) => Some(s + value)
      }
    }
  }

  private def toposort(rems: List[CompiledReactive]): List[CompiledReactive] = {
    val sorted = ArrayBuffer[CompiledReactive]()
    val discovered = scala.collection.mutable.HashSet[CompiledReactive]()

    def _toposort(rem: CompiledReactive): Unit = {
      if discovered.contains(rem) then ()
      else {
        discovered += rem
        dataflow.getOrElse(rem, Set()).foreach(_toposort)
        sorted += rem
      }
    }

    rems.foreach(_toposort)
    sorted.toList
  }

  private val updateFunctions: List[CFunctionDecl] = reactives.map(_.updateFun)

  private val signalVariables: Map[CompiledSignal, CVarDecl] = signals.map {
    r => r -> CVarDecl(r.name, r.cType, None)
  }.toMap

  private val startup: CFunctionDecl = {
    val initSignals = signalVariables.collect {
      case (s@CompiledSignalExpr(_, updateFun, typeRepr), varDecl) =>
        CExprStmt(CAssignmentExpr(
          varDecl.ref,
          retain(
            CCallExpr(
              updateFun.ref,
              reactiveInputs(s) collect { case s: CompiledSignal => signalVariables(s).ref }
            ),
            typeRepr.asInstanceOf[TypeRepr]
          )
        ))
      case (CompiledFold(_, init, _, _, typeRepr), varDecl) =>
        CExprStmt(CAssignmentExpr(
          varDecl.ref,
          retain(init, typeRepr.asInstanceOf[TypeRepr])
        ))
    }.toList

    val initGlobalVals = ctx.valueDeclList.collect[CStmt] {
      case v @ CVarDecl(_, _, Some(init)) => CAssignmentExpr(v.ref, init)
    }

    CFunctionDecl(
      appName + "_startup",
      List(),
      CVoidType,
      Some(CCompoundStmt(
        initGlobalVals ++ initSignals
      ))
    )
  }

  private val sourceParameters: Map[CompiledReactive, CParmVarDecl] = sources.collect {
    case s: CompiledEvent if s.isSource => s -> CParmVarDecl(s.name, s.cType)
    case s: CompiledSignal if s.isSource => s ->
      CParmVarDecl(
        s.name + "_param",
        cascade.dispatch(_.compileTypeRepr)(TypeRepr.of[Option].appliedTo(s.typeRepr))
      )
  }.toMap

  private def eventVar(r: CompiledEvent): CVarDecl =
    CVarDecl(
      r.name,
      r.cType,
      Some(CDesignatedInitExpr(List("defined" -> CFalseLiteral)))
    )

  private val eventVariables: Map[CompiledEvent, CVarDecl] = events.collect {
    case r if r.isSource && cascade.dispatch(_.usesRefCount)(r.typeRepr) =>
      r -> eventVar(r).copy(name = r.name + "_copy")
    case r if !r.isSource && r.cType != CQualType(CVoidType) => r -> eventVar(r)
  }.toMap

  private val signalChangedVars: Map[CompiledSignal, CVarDecl] = signals.map {
    s => s -> CVarDecl(s.name + "_changed", StdBoolH.bool, Some(CFalseLiteral))
  }.toMap

  private val jsonVars: Map[CompiledReactive, CVarDecl] = outputReactives.map {
    r => r -> CVarDecl(r.name + "_json", CPointerType(CJSONH.cJSON))
  }.toMap

  extension (expr: CExpr)
    def defined: CExpr = CMemberExpr(expr, "defined")
    def value: CExpr = CMemberExpr(expr, "val")

  private def conditionAfterFilter(r: CompiledReactive, cond: Map[CompiledReactive, Set[CExpr]]): Set[CExpr] = r match {
    case s: CompiledSignal => Set(signalChangedVars(s).ref)
    case s if s.isSource => cond(s)
    case e: CompiledEvent if !e.alwaysPropagates => Set(eventVariables(e).ref.defined)
    case _ => cond(r)
  }

  private val updateConditions: Map[CompiledReactive, Set[CExpr]] =
    topological.foldLeft(Map.empty[CompiledReactive, Set[CExpr]]) { (acc, r) =>
      r match {
        case s if s.isSource => acc.updated(s, Set(sourceParameters(s).ref.defined))
        case f: CompiledFold => acc.updated(f, conditionAfterFilter(reactiveInputs(f).head, acc))
        case _ => acc.updated(r, reactiveInputs(r).map(conditionAfterFilter(_, acc)).reduce(_ union _))
      }
    }

  private def compileCondition(conds: Set[CExpr]): CExpr = conds.toList match {
    case List(c) => c
    case l => l.reduce(COrExpr.apply)
  }

  private val topologicalByCond: List[CompiledReactive] = {
    val (groupedByCond: Map[Set[CExpr], List[CompiledReactive]], condOrder: List[Set[CExpr]]) =
      topological.foldLeft((Map.empty[Set[CExpr], List[CompiledReactive]], List[Set[CExpr]]())) {
        case ((groupedByCond, condOrder), r) =>
          val cond = updateConditions(r)
          if groupedByCond.contains(cond) then (groupedByCond.updated(cond, groupedByCond(cond) :+ r), condOrder)
          else (groupedByCond.updated(cond, List(r)), condOrder :+ cond)
      }

    condOrder.flatMap(groupedByCond)
  }

  private def valueRef(r: CompiledReactive): CDeclRefExpr =
    r match {
      case s: CompiledSignal => signalVariables(s).ref
      case s if s.isSource && !cascade.dispatch(_.usesRefCount)(s.typeRepr) => sourceParameters(s).ref
      case e: CompiledEvent => eventVariables(e).ref
    }

  private def compileUpdates(remainingReactives: List[CompiledReactive], toRelease: Set[CompiledReactive]): List[CStmt] = {
    if remainingReactives.isEmpty then return Nil

    val condition = updateConditions(remainingReactives.head)
    val (sameCond: List[CompiledReactive], otherConds: List[CompiledReactive]) =
      remainingReactives.span { r => updateConditions(r) == condition }

    def eventUpdateAssignment(event: CompiledReactive, rhs: CExpr): CStmt =
      CAssignmentExpr(
        valueRef(event),
        retain(rhs, event.typeRepr)
      )

    def signalUpdateAssignment(signal: CompiledSignal, rhs: CExpr): CStmt = {
      val tempDecl = CVarDecl("temp", signal.cType, Some(valueRef(signal)))
      val oldDecl = CVarDecl("_old", signal.cType, Some(retain(deepCopy(valueRef(signal), signal.typeRepr), signal.typeRepr)))

      val changedTest = cascade.dispatchLifted(_.compileEquals)(oldDecl.ref, signal.typeRepr, valueRef(signal), signal.typeRepr) match {
        case Some(expr) => CNotExpr(CParenExpr(expr))
        case None => CTrueLiteral
      }

      CCompoundStmt(List[CStmt](
        tempDecl,
        oldDecl,
        CAssignmentExpr(
          valueRef(signal),
          retain(rhs, signal.typeRepr)
        ),
        CAssignmentExpr(
          signalChangedVars(signal).ref,
          changedTest
        )
      ) ++ release(oldDecl.ref, signal.typeRepr, CFalseLiteral) ++ release(tempDecl.ref, signal.typeRepr, CFalseLiteral))
    }

    val updates = sameCond.collect {
      case s: CompiledEvent if s.isSource && cascade.dispatch(_.usesRefCount)(s.typeRepr) =>
        eventUpdateAssignment(s, deepCopy(sourceParameters(s).ref, s.typeRepr))
      case s: CompiledSignal if s.isSource =>
        signalUpdateAssignment(s, deepCopy(sourceParameters(s).ref.value, s.typeRepr))
      case f: CompiledFold =>
        signalUpdateAssignment(
          f,
          CCallExpr(
            f.updateFun.ref,
            valueRef(f) :: valueRef(reactiveInputs(f).head).value :: reactiveInputs(f).tail.map(valueRef)
          )
        )
      case s: CompiledSignalExpr =>
        signalUpdateAssignment(
          s,
          CCallExpr(
            s.updateFun.ref,
            reactiveInputs(s).map(valueRef)
          )
        )
      case e: CompiledEvent if e.cType == CVoidType =>
        CExprStmt(CCallExpr(e.updateFun.ref, reactiveInputs(e).map(valueRef)))
      case e: CompiledEvent if !e.isSource =>
        eventUpdateAssignment(
          e,
          CCallExpr(
            e.updateFun.ref,
            reactiveInputs(e).map(valueRef)
          )
        )
    }

    val sameCondCode = CIfStmt(compileCondition(condition), CCompoundStmt(updates))

    val stillUsed = otherConds ++ otherConds.flatMap(reactiveInputs)
    val released = toRelease.filterNot(stillUsed.contains)

    val serialization = CEmptyStmt :: sameCond.collect[CStmt] {
      case e: CompiledEvent if jsonVars.contains(e) =>
        CAssignmentExpr(jsonVars(e).ref, serialize(valueRef(e), e.typeRepr))
      case s: CompiledSignal if jsonVars.contains(s) =>
        CAssignmentExpr(
          jsonVars(s).ref,
          CConditionalOperator(
            signalChangedVars(s).ref,
            serialize(valueRef(s), s.typeRepr),
            CCallExpr(CJSONH.cJSON_CreateNull.ref, List())
          )
        )
    }

    val releaseCode = released.flatMap {
      case r if cascade.dispatch(_.usesRefCount)(r.typeRepr) =>
        CEmptyStmt :: (release(valueRef(r), r.typeRepr, CFalseLiteral) map { releaseStmt =>
          CIfStmt(valueRef(r).defined, releaseStmt)
        }).toList
      case _ => None
    }

    (CEmptyStmt :: sameCondCode :: (serialization ++ releaseCode.toList)) ++ compileUpdates(otherConds, toRelease.diff(released))
  }

  private val updateFunction: CFunctionDecl = {
    val jsonParam = CParmVarDecl("json", CPointerType(CJSONH.cJSON))
    val params = jsonParam :: sourcesTopological.map(sourceParameters)

    val localVarDecls = topological.collect {
      case e: CompiledEvent if eventVariables.contains(e) => CDeclStmt(eventVariables(e))
      case s: CompiledSignal => CDeclStmt(signalChangedVars(s))
    }

    val jsonVarDecls = CEmptyStmt :: jsonVars.values.toList.map(CDeclStmt.apply)

    val updates = compileUpdates(topologicalByCond, eventVariables.keySet.toSet[CompiledReactive])

    val fillJson = CEmptyStmt ::
      jsonVars.values.map[CStmt](jsonVar => CCallExpr(CJSONH.cJSON_AddItemToArray.ref, List(jsonParam.ref, jsonVar.ref))).toList

    val body = CCompoundStmt(localVarDecls ++ jsonVarDecls ++ updates ++ fillJson)

    CFunctionDecl(appName + "_update", params, CVoidType, Some(body))
  }

  private val onData: CFunctionDecl = {
    val eDecl = CParmVarDecl("e", CPointerType(DyadH.dyad_Event))

    val jsonDecl = CVarDecl(
      "json",
      CPointerType(CJSONH.cJSON),
      Some(CCallExpr(CJSONH.cJSON_Parse.ref, List(CMemberExpr(eDecl.ref, DyadH.dataField, true))))
    )

    val outputMsgDecl = CVarDecl("outputMsg", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_CreateArray.ref, List())))

    val deserializedSources = sourcesTopological.zipWithIndex.map {
      case (e: CompiledEvent, i) =>
        deserialize(CCallExpr(CJSONH.cJSON_GetArrayItem.ref, List(jsonDecl.ref, i.lit)), e.typeRepr)
      case (s: CompiledSignal, i) =>
        deserialize(CCallExpr(CJSONH.cJSON_GetArrayItem.ref, List(jsonDecl.ref, i.lit)), TypeRepr.of[Option].appliedTo(s.typeRepr))
    }

    val writeOutput = CCallExpr(
      DyadH.dyad_writef.ref,
      List(
        CMemberExpr(eDecl.ref, DyadH.streamField, true),
        CStringLiteral("%s\\n"),
        CCallExpr(CJSONH.cJSON_Print.ref, List(outputMsgDecl.ref))
      )
    )

    CFunctionDecl(
      "onData",
      List(eDecl),
      CVoidType,
      Some(CCompoundStmt(List(
        jsonDecl,
        outputMsgDecl,
        CCallExpr(updateFunction.ref, outputMsgDecl.ref :: deserializedSources),
        writeOutput,
        CCallExpr(CJSONH.cJSON_Delete.ref, List(outputMsgDecl.ref)),
        CCallExpr(CJSONH.cJSON_Delete.ref, List(jsonDecl.ref))
      )))
    )
  }

  private val onAccept: CFunctionDecl = {
    val eDecl = CParmVarDecl("e", CPointerType(DyadH.dyad_Event))

    CFunctionDecl(
      "onAccept",
      List(eDecl),
      CVoidType,
      Some(CCompoundStmt(List(
        CCallExpr(
          DyadH.dyad_addListener.ref,
          List(
            CMemberExpr(eDecl.ref, DyadH.remoteField, true),
            DyadH.DYAD_EVENT_DATA.ref,
            onData.ref,
            CNullLiteral
          )
        )
      )))
    )
  }

  private val mainFun: CFunctionDecl = {
    val argc = CParmVarDecl("argc", CIntegerType)
    val argv = CParmVarDecl("argv", CArrayType(CPointerType(CCharType)))

    val checkArgs = CIfStmt(
      CLessThanExpr(argc.ref, 2.lit),
      CCompoundStmt(List(
        CompileString.printf("Listen port expected as command line argument\\n"),
        CReturnStmt(Some(1.lit))
      ))
    )

    val streamDecl = CVarDecl("s", CPointerType(DyadH.dyad_Stream), Some(CCallExpr(DyadH.dyad_newStream.ref, List())))
    val registerOnAccept = CCallExpr(
      DyadH.dyad_addListener.ref,
      List(streamDecl.ref, DyadH.DYAD_EVENT_ACCEPT.ref, onAccept.ref, CNullLiteral)
    )
    val startListening = CCallExpr(
      DyadH.dyad_listen.ref,
      List(
        streamDecl.ref,
        CCallExpr(StdLibH.atoi.ref, List(CArraySubscriptExpr(argv.ref, 1.lit))))
    )

    val dyadUpdateLoop = CWhileStmt(
      CGreaterThanExpr(CCallExpr(DyadH.dyad_getStreamCount.ref, List()), 0.lit),
      CCompoundStmt(List(CCallExpr(DyadH.dyad_update.ref, List())))
    )

    val releaseSignals = signals.flatMap(s => release(valueRef(s), s.typeRepr, CFalseLiteral))
    val releaseGlobals = ctx.valueDeclList.collect {
      case v: CVarDecl => release(v, CFalseLiteral)
    }.flatten

    CFunctionDecl(
      "main",
      List(argc, argv),
      CIntegerType,
      Some(CCompoundStmt(
        List[CStmt](
          checkArgs, CEmptyStmt,
          CCallExpr(startup.ref, List()),
          CCallExpr(DyadH.dyad_init.ref, List()), CEmptyStmt,
          streamDecl,
          registerOnAccept,
          startListening, CEmptyStmt,
          dyadUpdateLoop, CEmptyStmt,
          CCallExpr(DyadH.dyad_shutdown.ref, List())
        ) ++ releaseSignals
          ++ releaseGlobals
          :+ CReturnStmt(Some(0.lit))
      ))
    )
  }

  private val mainC: String = appName + "Main.c"
  private val mainH: String = appName + "Main.h"
  private val mainInclude: CInclude = CInclude(mainH, true)
  private val libC: String = appName + "Lib.c"
  private val libH: String = appName + "Lib.h"
  private val libInclude: CInclude = CInclude(libH, true)
  private val appC: String = appName + "App.c"
  private val appOut: String = appName + "App"

  private val mainCTU: CTranslationUnitDecl = {
    val includes = mainInclude :: libInclude :: ctx.includesList

    val globalVarDecls = topological.collect {
      case s: CompiledSignal => signalVariables(s)
    }

    CTranslationUnitDecl(
      includes,
      globalVarDecls ++ updateFunctions ++ List(startup, updateFunction)
    )
  }

  private val mainHTU: CTranslationUnitDecl = {
    val includes = libInclude :: ctx.includesList

    val globalVarDecls = topological.collect {
      case s: CompiledSignal => signalVariables(s)
    }

    CTranslationUnitDecl(
      includes,
      globalVarDecls ++ List(startup.declOnly, updateFunction.declOnly),
      Some(appName.toUpperCase + "_MAIN")
    )
  }

  private def writeFile(path: String, content: String): Unit = {
    val file = new File(path)
    file.createNewFile()

    val fileWriter = new FileWriter(path)
    fileWriter.write(content)
    fileWriter.close()
  }

  private def writeMain(pathToDir: String): Unit = {
    writeFile(pathToDir + "/" + mainC, mainCTU.textgen)
    writeFile(pathToDir + "/" + mainH, mainHTU.textgen)
  }

  private val libCTU: CTranslationUnitDecl =
    CTranslationUnitDecl(
      libInclude :: ctx.includesList,
      ctx.valueDeclList.sortBy(_.name) map {
        case CVarDecl(name, declaredType, Some(_)) => CVarDecl(name, declaredType)
        case other => other
      }
    )

  private val libHTU: CTranslationUnitDecl = {
    val valueDecls = ctx.valueDeclList.map(_.declOnly).sortBy(_.name)

    CTranslationUnitDecl(
      ctx.includesList,
      ctx.typeDeclList ++ valueDecls,
      Some(appName.toUpperCase + "_LIB")
    )
  }

  private def writeLib(pathToDir: String): Unit = {
    writeFile(pathToDir + "/" + libC, libCTU.textgen)
    writeFile(pathToDir + "/" + libH, libHTU.textgen)
  }

  private val appCTU: CTranslationUnitDecl =
    CTranslationUnitDecl(
      List(mainInclude, libInclude) ++ ctx.includesList,
      List(onData, onAccept, mainFun)
    )

  private def writeApp(pathToDir: String): Unit = {
    writeFile(pathToDir + "/" + appC, appCTU.textgen)
  }

  private def writeMakeFile(pathToDir: String, compiler: String): Unit = {
    val localIncludes = ctx.includesList.filter(_.isLocal)

    val localFileString = localIncludes.flatMap { incl =>
      List(incl.name, incl.name.init + "c")
    }.mkString("", " ", "")

    val content =
      s"""
         |build:
         |\t$compiler $appC $mainC $mainH $libC $libH $localFileString -o $appOut
      """.strip().stripMargin

    writeFile(pathToDir + "/Makefile", content)
  }

  def writeIntoDir(pathToDir: String, compiler: String): Unit = {
    writeMain(pathToDir)
    writeLib(pathToDir)
    writeApp(pathToDir)
    writeMakeFile(pathToDir, compiler)
  }
}
