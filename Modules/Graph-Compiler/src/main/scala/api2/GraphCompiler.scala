package api2

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.{CAndExpr, CAssignmentExpr, CEqualsExpr, CGreaterThanExpr, CLessThanExpr, CNotEqualsExpr, COrExpr}
import clangast.expr.unaryop.CNotExpr
import clangast.stmt.*
import clangast.stubs.{CJSONH, DyadH, StdBoolH, StdLibH}
import clangast.types.*
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.context.RecordDeclTC
import compiler.base.*
import compiler.base.DataStructureFragment.{deepCopy, release, retain}
import compiler.ext.*
import compiler.ext.SerializationFragment.{deserialize, serialize}

import java.io.{File, FileWriter}
import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer
import scala.quoted.*

class GraphCompiler(using Quotes)(
    reactives: List[CompiledReactive],
    externalSources: List[CompiledReactive],
    outputReactives: List[CompiledReactive],
    appName: String
)(using fc: FragmentedCompiler)(using ctx: RecordDeclTC) {
  import quotes.reflect.*

  private val sources: List[CompiledReactive] = reactives.filter(_.isSource)
  private val signals: List[CompiledSignal]   = reactives.collect { case s: CompiledSignal => s }
  private val events: List[CompiledEvent]     = reactives.collect { case e: CompiledEvent => e }

  private val nameToReactive: Map[String, CompiledReactive] = reactives.map(r => r.name -> r).toMap
  private val reactiveInputs: Map[CompiledReactive, List[CompiledReactive]] =
    reactives.map(r => r -> r.inputs.map(nameToReactive)).toMap
  private val dataflow: Map[CompiledReactive, Set[CompiledReactive]] =
    reactives.foldLeft(Map.empty[CompiledReactive, Set[CompiledReactive]]) { (acc, r) =>
      addValueToAllKeys(acc, reactiveInputs(r), r)
    }

  private val topological: List[CompiledReactive] = toposort(sources).reverse
  private val localSourcesTopological: List[CompiledReactive] =
    topological.filter(r => r.isSource && !externalSources.contains(r))
  private val orderedSources: List[CompiledReactive] = localSourcesTopological ++ externalSources

  private val hasExternalSources: Boolean = externalSources.nonEmpty
  private val hasOutputReactives: Boolean = outputReactives.nonEmpty
  private val isConnected: Boolean        = hasExternalSources || hasOutputReactives

  private def addValueToAllKeys[K, V](originalMap: Map[K, Set[V]], keys: List[K], value: V): Map[K, Set[V]] = {
    keys.foldLeft(originalMap) { (acc, i) =>
      acc.updatedWith(i) {
        case None    => Some(Set(value))
        case Some(s) => Some(s + value)
      }
    }
  }

  private def toposort(rems: List[CompiledReactive]): List[CompiledReactive] = {
    val sorted     = ArrayBuffer[CompiledReactive]()
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

  private def computeSubGraph(startNodes: Set[CompiledReactive]): Set[CompiledReactive] = {
    if (startNodes.isEmpty) {
      Set()
    } else {
      val activatedNext = startNodes.flatMap { n =>
        dataflow.getOrElse(n, Set()).filter {
          case f: CompiledFold if nameToReactive(f.primaryInput) != n => false
          case _                                                      => true
        }
      }
      startNodes union computeSubGraph(activatedNext)
    }
  }

  private val updateFunctions: List[CFunctionDecl] = reactives.collect {
    case s: CompiledSignal => s.updateFun
    case r if !r.isSource  => r.updateFun
  }

  private val signalVariables: Map[CompiledSignal, CVarDecl] = signals.map {
    r => r -> CVarDecl(r.name, r.cType, None)
  }.toMap

  private val startup: CFunctionDecl = {
    val initSignals = signalVariables.collect {
      case (s @ CompiledSignalExpr(_, updateFun, typeRepr), varDecl) =>
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
      case v @ CVarDecl(_, _, Some(init), _) => CAssignmentExpr(v.ref, init)
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
    case s: CompiledSignal if s.isSource =>
      s ->
      CParmVarDecl(
        s.name + "_param",
        dispatch[TypeIFFragment](_.compileTypeRepr)(TypeRepr.of[Option].appliedTo(s.typeRepr))
      )
  }.toMap

  private def eventVar(r: CompiledEvent): CVarDecl =
    CVarDecl(
      r.name,
      r.cType,
      Some(CDesignatedInitExpr(List("defined" -> CFalseLiteral)))
    )

  private val eventVariables: Map[CompiledEvent, CVarDecl] = events.collect {
    case r if r.isSource && dispatch[DataStructureIFFragment](_.usesRefCount)(r.typeRepr) =>
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
    def value: CExpr   = CMemberExpr(expr, "val")

  private def conditionAfterFilter(r: CompiledReactive, cond: Map[CompiledReactive, Set[CExpr]]): Set[CExpr] = r match {
    case s: CompiledSignal                       => Set(signalChangedVars(s).ref)
    case s if s.isSource                         => cond(s)
    case e: CompiledEvent if !e.alwaysPropagates => Set(eventVariables(e).ref.defined)
    case _                                       => cond(r)
  }

  private def updateConditions(subGraph: Set[CompiledReactive]): Map[CompiledReactive, Set[CExpr]] =
    topological.filter(subGraph.contains).foldLeft(Map.empty[CompiledReactive, Set[CExpr]]) { (acc, r) =>
      r match {
        case s if s.isSource => acc.updated(s, Set(sourceParameters(s).ref.defined))
        case f: CompiledFold => acc.updated(f, conditionAfterFilter(reactiveInputs(f).head, acc))
        case _ => acc.updated(
            r,
            reactiveInputs(r).filter(subGraph.contains).map(conditionAfterFilter(_, acc)).reduce(_ union _)
          )
      }
    }

  private def compileCondition(conds: Set[CExpr]): CExpr = conds.toList match {
    case List(c) => c
    case l       => l.reduce(COrExpr.apply)
  }

  private def topologicalByCond(
      subGraph: Set[CompiledReactive],
      subGraphConds: Map[CompiledReactive, Set[CExpr]]
  ): List[CompiledReactive] = {
    val (groupedByCond: Map[Set[CExpr], List[CompiledReactive]], condOrder: List[Set[CExpr]]) =
      topological.filter(subGraph.contains).foldLeft((
        Map.empty[Set[CExpr], List[CompiledReactive]],
        List[Set[CExpr]]()
      )) {
        case ((groupedByCond, condOrder), r) =>
          val cond = subGraphConds(r)
          if groupedByCond.contains(cond) then (groupedByCond.updated(cond, groupedByCond(cond) :+ r), condOrder)
          else (groupedByCond.updated(cond, List(r)), condOrder :+ cond)
      }

    condOrder.flatMap(groupedByCond)
  }

  private def valueRef(r: CompiledReactive): CDeclRefExpr =
    r match {
      case s: CompiledSignal                                                                 => signalVariables(s).ref
      case s if s.isSource && !dispatch[DataStructureIFFragment](_.usesRefCount)(s.typeRepr) => sourceParameters(s).ref
      case e: CompiledEvent                                                                  => eventVariables(e).ref
    }

  private def compileUpdates(
      remainingReactives: List[CompiledReactive],
      subGraphConds: Map[CompiledReactive, Set[CExpr]],
      toRelease: Set[CompiledEvent]
  ): List[CStmt] = {
    if remainingReactives.isEmpty then return Nil

    val condition = subGraphConds(remainingReactives.head)
    val (sameCond: List[CompiledReactive], otherConds: List[CompiledReactive]) =
      remainingReactives.span { r => subGraphConds(r) == condition }

    def eventUpdateAssignment(event: CompiledReactive, rhs: CExpr): CStmt =
      CAssignmentExpr(
        valueRef(event),
        retain(rhs, event.typeRepr)
      )

    def signalUpdateAssignment(signal: CompiledSignal, rhs: CExpr): CStmt = {
      val tempDecl = CVarDecl("temp", signal.cType, Some(valueRef(signal)))
      val oldDecl =
        CVarDecl("_old", signal.cType, Some(retain(deepCopy(valueRef(signal), signal.typeRepr), signal.typeRepr)))

      val changedTest = fc.dispatchLifted[ApplyIFFragment](_.compileEquals)(
        oldDecl.ref,
        signal.typeRepr,
        valueRef(signal),
        signal.typeRepr
      ) match {
        case Some(expr) => CNotExpr(CParenExpr(expr))
        case None       => CTrueLiteral
      }

      CCompoundStmt(
        (if dispatch[DataStructureIFFragment](_.usesRefCount)(signal.typeRepr) then List[CStmt](tempDecl) else Nil) ++
        List[CStmt](
          oldDecl,
          CAssignmentExpr(
            valueRef(signal),
            retain(rhs, signal.typeRepr)
          ),
          CAssignmentExpr(
            signalChangedVars(signal).ref,
            changedTest
          )
        ) ++ release(oldDecl.ref, signal.typeRepr, CFalseLiteral) ++ release(
          tempDecl.ref,
          signal.typeRepr,
          CFalseLiteral
        )
      )
    }

    def refOrNone(r: CompiledReactive): CExpr = r match {
      case ev: CompiledEvent if !subGraphConds.contains(ev) =>
        CCallExpr(OptionFragment.getNoneCreator(ev.typeRepr).ref, List())
      case _ => valueRef(r)
    }

    val updates = sameCond.collect {
      case s: CompiledEvent if s.isSource && dispatch[DataStructureIFFragment](_.usesRefCount)(s.typeRepr) =>
        eventUpdateAssignment(s, deepCopy(sourceParameters(s).ref, s.typeRepr))
      case s: CompiledSignal if s.isSource =>
        signalUpdateAssignment(s, deepCopy(sourceParameters(s).ref.value, s.typeRepr))
      case f: CompiledFold =>
        signalUpdateAssignment(
          f,
          CCallExpr(
            f.updateFun.ref,
            valueRef(f) :: refOrNone(reactiveInputs(f).head).value :: reactiveInputs(f).tail.map(refOrNone)
          )
        )
      case s: CompiledSignalExpr =>
        signalUpdateAssignment(
          s,
          CCallExpr(
            s.updateFun.ref,
            reactiveInputs(s).map(refOrNone)
          )
        )
      case e: CompiledEvent if e.cType == CVoidType =>
        CExprStmt(CCallExpr(e.updateFun.ref, reactiveInputs(e).map(refOrNone)))
      case e: CompiledEvent if !e.isSource =>
        eventUpdateAssignment(
          e,
          CCallExpr(
            e.updateFun.ref,
            reactiveInputs(e).map(refOrNone)
          )
        )
    }

    val sameCondCode = CIfStmt(compileCondition(condition), CCompoundStmt(updates))

    val stillUsed = otherConds ++ otherConds.flatMap(reactiveInputs)
    val released  = toRelease.filterNot(stillUsed.contains)

    @nowarn
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
      case r if dispatch[DataStructureIFFragment](_.usesRefCount)(r.typeRepr) =>
        CEmptyStmt :: (release(valueRef(r), r.typeRepr, CFalseLiteral) map { releaseStmt =>
          CIfStmt(valueRef(r).defined, releaseStmt)
        }).toList
      case _ => None
    }

    (CEmptyStmt :: sameCondCode :: ( /*serialization ++ */ releaseCode.toList)) ++ compileUpdates(
      otherConds,
      subGraphConds,
      toRelease.diff(released)
    )
  }

  private def emptySourceArgs(reactives: List[CompiledReactive]): List[CExpr] = reactives.map {
    case e: CompiledEvent => CCallExpr(OptionFragment.getNoneCreator(e.typeRepr).ref, List())
    case s: CompiledSignal =>
      CCallExpr(OptionFragment.getNoneCreator(TypeRepr.of[Option].appliedTo(s.typeRepr)).ref, List())
  }

  private def buildTransactionFunction(sources: Set[CompiledReactive], nameSuffix: String): CFunctionDecl = {
    val fullName = s"${appName}_transaction_$nameSuffix"

    val subGraph            = computeSubGraph(sources)
    val subGraphConds       = updateConditions(subGraph)
    val subGraphTopological = topologicalByCond(subGraph, subGraphConds)

    val sourceParams = orderedSources.filter(sources.contains).map(sourceParameters)

    val params = sourceParams
    // if outputReactives.nonEmpty
    // then CParmVarDecl("json", CPointerType(CJSONH.cJSON)) :: sourceParams
    // else sourceParams

    val localVarDecls = subGraphTopological.collect {
      case e: CompiledEvent if eventVariables.contains(e) => CDeclStmt(eventVariables(e))
      case s: CompiledSignal if subGraph.contains(s)      => CDeclStmt(signalChangedVars(s))
    }

    val toRelease = eventVariables.keySet.filter(subGraph.contains)

    val updates = compileUpdates(subGraphTopological, subGraphConds, toRelease)

    val returnType      = CRecordDecl(s"${fullName}_result", outputReactives.map { r => CFieldDecl(r.name, r.cType) })
    val productCreation = ProductFragment.makeProductCreator(returnType)

    ctx.addTypeDecl(returnType)
    ctx.registerRecordFun(returnType, "CREATE", productCreation)

    val body =
      if outputReactives.isEmpty
      then CCompoundStmt(localVarDecls ++ updates)
      else {
        // val subGraphJsonVars = outputReactives.map { r =>
        //  if subGraph.contains(r) then
        //    jsonVars(r)
        //  else
        //    jsonVars(r).copy(init = Some(CCallExpr(CJSONH.cJSON_CreateNull.ref, List())))
        // }
        // val jsonVarDecls = CEmptyStmt :: subGraphJsonVars.map(CDeclStmt.apply)

        // val fillJson = CEmptyStmt ::
        //  subGraphJsonVars.map[CStmt](jsonVar =>
        //    CCallExpr(CJSONH.cJSON_AddItemToArray.ref, List(params.head.ref, jsonVar.ref))
        //  )

        // CCompoundStmt(localVarDecls ++ jsonVarDecls ++ updates ++ fillJson)

        val result = List(CReturnStmt(Some(CCallExpr(productCreation.ref, outputReactives.map(valueRef)))))

        CCompoundStmt(localVarDecls ++ updates ++ result)
      }

    CFunctionDecl(fullName, params, returnType.getTypeForDecl, Some(body))
  }

  val localTransactionFunction: CFunctionDecl = buildTransactionFunction(localSourcesTopological.toSet, "local")

  val remoteTransactionFunction: CFunctionDecl = buildTransactionFunction(externalSources.toSet, "remote")

  val transactionFunctions: List[CFunctionDecl] = {
    val localFun = if localSourcesTopological.nonEmpty then List(localTransactionFunction) else List()

    val remoteFun = if externalSources.nonEmpty then List(remoteTransactionFunction) else List()

    localFun ++ remoteFun
  }

  lazy val onData: CFunctionDecl = {
    val eDecl = CParmVarDecl("e", CPointerType(DyadH.dyad_Event))

    val jsonDecl = CVarDecl(
      "json",
      CPointerType(CJSONH.cJSON),
      Some(CCallExpr(CJSONH.cJSON_Parse.ref, List(CMemberExpr(eDecl.ref, DyadH.dataField, true))))
    )

    val deserializedSources = externalSources.zipWithIndex.map {
      case (e: CompiledEvent, i) =>
        deserialize(CCallExpr(CJSONH.cJSON_GetArrayItem.ref, List(jsonDecl.ref, i.lit)), e.typeRepr)
      case (s: CompiledSignal, i) =>
        deserialize(
          CCallExpr(CJSONH.cJSON_GetArrayItem.ref, List(jsonDecl.ref, i.lit)),
          TypeRepr.of[Option].appliedTo(s.typeRepr)
        )
    }

    val body = if (hasOutputReactives) {
      val outputMsgDecl =
        CVarDecl("outputMsg", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_CreateArray.ref, List())))

      val writeOutput = CCallExpr(
        DyadH.dyad_writef.ref,
        List(
          CMemberExpr(eDecl.ref, DyadH.streamField, true),
          CStringLiteral("%s\\n"),
          CCallExpr(CJSONH.cJSON_Print.ref, List(outputMsgDecl.ref))
        )
      )

      val callRemoteTx = CCallExpr(remoteTransactionFunction.ref, outputMsgDecl.ref :: deserializedSources)

      val transactionResultDcl = CVarDecl("transactionResult", remoteTransactionFunction.returnType, Some(callRemoteTx))

      CCompoundStmt(List(
        jsonDecl,
        outputMsgDecl,
        transactionResultDcl,
        writeOutput,
        CCallExpr(CJSONH.cJSON_Delete.ref, List(outputMsgDecl.ref)),
        CCallExpr(CJSONH.cJSON_Delete.ref, List(jsonDecl.ref))
      ))
    } else {
      CCompoundStmt(List(
        jsonDecl,
        CCallExpr(remoteTransactionFunction.ref, deserializedSources),
        CCallExpr(CJSONH.cJSON_Delete.ref, List(jsonDecl.ref))
      ))
    }

    CFunctionDecl("onData", List(eDecl), CVoidType, Some(body))
  }

  lazy val clientStream: CVarDecl = CVarDecl("clientStream", CPointerType(DyadH.dyad_Stream))

  lazy val onAccept: CFunctionDecl = {
    val eDecl              = CParmVarDecl("e", CPointerType(DyadH.dyad_Event))
    val assignClientStream = CAssignmentExpr(clientStream.ref, CMemberExpr(eDecl.ref, DyadH.remoteField, true))

    val body = if (hasExternalSources) {
      CCompoundStmt(List(
        CCallExpr(
          DyadH.dyad_addListener.ref,
          List(
            CMemberExpr(eDecl.ref, DyadH.remoteField, true),
            DyadH.DYAD_EVENT_DATA.ref,
            onData.ref,
            CNullLiteral
          )
        ),
        assignClientStream
      ))
    } else {
      CCompoundStmt(List(assignClientStream))
    }

    CFunctionDecl("onAccept", List(eDecl), CVoidType, Some(body))
  }

  private val mainFun: CFunctionDecl = {
    val argc = CParmVarDecl("argc", CIntegerType)
    val argv = CParmVarDecl("argv", CArrayType(CPointerType(CCharType)))

    val releaseSignals = signals.flatMap(s => release(valueRef(s), s.typeRepr, CFalseLiteral))
    val releaseGlobals = ctx.valueDeclList.collect {
      case v: CVarDecl => release(v, CFalseLiteral)
    }.flatten

    val body = if (isConnected) {
      val checkArgs = CIfStmt(
        CLessThanExpr(argc.ref, 2.lit),
        CCompoundStmt(List(
          StringFragment.printf("Listen port expected as command line argument\\n"),
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
          CCallExpr(StdLibH.atoi.ref, List(CArraySubscriptExpr(argv.ref, 1.lit)))
        )
      )

      val updateFromLocalSources = if (!hasExternalSources) {
        val outputMsgDecl =
          CVarDecl("outputMsg", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_CreateArray.ref, List())))

        val writeOutput = CIfStmt(
          CAndExpr(
            CNotEqualsExpr(clientStream.ref, CNullLiteral),
            CEqualsExpr(CCallExpr(DyadH.dyad_getState.ref, List(clientStream.ref)), DyadH.DYAD_STATE_CONNECTED.ref)
          ),
          CCallExpr(
            DyadH.dyad_writef.ref,
            List(
              clientStream.ref,
              CStringLiteral("%s\\n"),
              CCallExpr(CJSONH.cJSON_Print.ref, List(outputMsgDecl.ref))
            )
          )
        )

        List[CStmt](
          outputMsgDecl,
          CCallExpr(localTransactionFunction.ref, outputMsgDecl.ref :: emptySourceArgs(localSourcesTopological)),
          writeOutput,
          CCallExpr(CJSONH.cJSON_Delete.ref, List(outputMsgDecl.ref))
        )
      } else List()

      val dyadUpdateLoop = CWhileStmt(
        CGreaterThanExpr(CCallExpr(DyadH.dyad_getStreamCount.ref, List()), 0.lit),
        CCompoundStmt(
          updateFromLocalSources :+ CExprStmt(CCallExpr(DyadH.dyad_update.ref, List()))
        )
      )

      CCompoundStmt(
        List[CStmt](
          checkArgs,
          CEmptyStmt,
          CCallExpr(startup.ref, List()),
          CCallExpr(DyadH.dyad_init.ref, List()),
          CEmptyStmt,
          streamDecl,
          registerOnAccept,
          startListening,
          CEmptyStmt,
          dyadUpdateLoop,
          CEmptyStmt,
          CCallExpr(DyadH.dyad_shutdown.ref, List())
        ) ++ releaseSignals
        ++ releaseGlobals
        :+ CReturnStmt(Some(0.lit))
      )
    } else {
      CCompoundStmt(
        List[CStmt](
          CCallExpr(startup.ref, List()),
          CCallExpr(localTransactionFunction.ref, emptySourceArgs(localSourcesTopological))
        ) ++ releaseSignals ++ releaseGlobals :+ CReturnStmt(Some(0.lit))
      )
    }

    CFunctionDecl("main", List(argc, argv), CIntegerType, Some(body))
  }

  def forUseInHeader(valueDecl: CValueDecl): CValueDecl = valueDecl match
    case cvd: CVarDecl => cvd.copy(inHeader = true)
    case other         => other

  private val mainC: String         = appName + "Main.c"
  private val mainH: String         = appName + "Main.h"
  private val mainInclude: CInclude = CInclude(mainH, true)
  private val appC: String          = appName + "App.c"
  private val appOut: String        = appName + "App"
  private val libC: String          = appName + "Lib.c"
  private val libH: String          = appName + "Lib.h"
  private val libInclude: CInclude  = CInclude(libH, true)

  private val mainCTU: CTranslationUnitDecl = {
    val includes = mainInclude :: libInclude :: ctx.includesList

    CTranslationUnitDecl(
      includes,
      updateFunctions ++ List(startup) ++ transactionFunctions
    )
  }

  private val mainHTU: CTranslationUnitDecl = {
    val includes = libInclude :: ctx.includesList

    val globalVarDecls = topological.collect {
      case s: CompiledSignal => signalVariables(s)
    }.map(forUseInHeader)

    CTranslationUnitDecl(
      includes,
      globalVarDecls ++ List(startup.declOnly) ++ transactionFunctions.map(_.declOnly),
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

  private val appCTU: CTranslationUnitDecl =
    CTranslationUnitDecl(
      List(mainInclude, libInclude) ++ ctx.includesList,
      if hasExternalSources then List(onData, clientStream, onAccept, mainFun)
      else if hasOutputReactives then List(clientStream, onAccept, mainFun)
      else List(mainFun)
    )

  private def writeApp(pathToDir: String): Unit = {
    writeFile(pathToDir + "/" + appC, appCTU.textgen)
  }

  private val libCTU: CTranslationUnitDecl =
    CTranslationUnitDecl(
      libInclude :: ctx.includesList,
      ctx.valueDeclList.filter {
        case _: CVarDecl => false
        case _           => true
      }.sortBy(_.name)
    )

  private val libHTU: CTranslationUnitDecl = {
    val valueDecls = ctx.valueDeclList.map(_.declOnly).sortBy(_.name).map(forUseInHeader)

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
    new File(pathToDir).mkdirs()
    writeMain(pathToDir)
    writeLib(pathToDir)
    writeApp(pathToDir)
    writeMakeFile(pathToDir, compiler)
  }
}
