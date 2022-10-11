package api2

import compiler.StandardReactiveMacroCompiler

import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.github.plokhotnyuk.jsoniter_scala.core.*

object CompileGraph {
  val macroCompiler: StandardReactiveMacroCompiler.type = StandardReactiveMacroCompiler

  export macroCompiler.compileGraph as isolated

  inline def withInput[IN <: Tuple : EventTupleUtils]
  (inline appName: String)(inline dependencies: IN)(inline graph: CEventsFromEvents[IN] => Unit): RemoteGraphWithInput[IN] = {

    macroCompiler.compileGraph(appName)(graph)

    given JsonValueCodec[OptionsFromEvents[IN]] = TupleCodecFactory.generateEventCodecs[IN]

    new RemoteGraphWithInput[IN] {
      override val events: IN = dependencies
    }
  }

  inline def withOutput[OUT <: Tuple]
  (inline appName: String)(inline graph: OUT)
  (using TupleUtils[TupleFromCEvents[OUT]], TupleCodec[TupleFromCEvents[OUT]]): RemoteGraphWithOutput[TupleFromCEvents[OUT]] = {

    macroCompiler.compileGraph(appName)(graph)

    given JsonValueCodec[OptionsFromTuple[TupleFromCEvents[OUT]]] =
      TupleCodecFactory.combineTupleCodecs(TupleCodecFactory.generateCReactiveCodecsTuple[OUT])

    new RemoteGraphWithOutput[TupleFromCEvents[OUT]] {}
  }

  inline def withIO[OUT <: Tuple, IN <: Tuple : EventTupleUtils]
  (inline appName: String)(inline dependencies: IN)(inline graph: CEventsFromEvents[IN] => OUT)
  (using TupleUtils[TupleFromCEvents[OUT]], TupleCodec[TupleFromCEvents[OUT]]): RemoteGraphWithIO[IN, TupleFromCEvents[OUT]] = {
    macroCompiler.compileGraph(appName)(graph)

    given JsonValueCodec[OptionsFromEvents[IN]] = TupleCodecFactory.generateEventCodecs[IN]

    given JsonValueCodec[OptionsFromTuple[TupleFromCEvents[OUT]]] =
      TupleCodecFactory.combineTupleCodecs(TupleCodecFactory.generateCReactiveCodecsTuple[OUT])

    new RemoteGraphWithIO[IN, TupleFromCEvents[OUT]] {
      override val events: IN = dependencies
    }
  }
}
