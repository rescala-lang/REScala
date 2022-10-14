package api2

import compiler.StandardReactiveMacroCompiler

import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.github.plokhotnyuk.jsoniter_scala.core.*

object CompileGraph {
  val macroCompiler: StandardReactiveMacroCompiler.type = StandardReactiveMacroCompiler

  export macroCompiler.compileGraph as isolated

  inline def withInput[IN <: Tuple : EventTupleUtils]
  (inline appName: String)(inline dependencies: IN)(inline graph: CEventsFromEvents[IN] => Unit)
  (using TupleCodec[TupleFromEvents[IN]]): RemoteGraphWithInput[IN] = {
    macroCompiler.compileGraph(appName)(graph)

    given JsonValueCodec[OptionsFromEvents[IN]] =
      TupleCodecFactory.combineTupleCodecs(TupleCodecFactory.generateEventCodecsTuple[IN]).asInstanceOf[JsonValueCodec[OptionsFromEvents[IN]]]

    new RemoteGraphWithInput[IN] {
      override val events: IN = dependencies
    }
  }

  inline def withOutput[OUT <: Tuple]
  (inline appName: String)(inline graph: OUT)
  (using TupleUtils[TupleFromCEvents[OUT]], TupleCodec[TupleFromCEvents[OUT]]): RemoteGraphWithOutput[TupleFromCEvents[OUT]] = {
    macroCompiler.compileGraph(appName)(graph)

    given JsonValueCodec[OptionsFromTuple[TupleFromCEvents[OUT]]] =
      TupleCodecFactory.combineTupleCodecs(TupleCodecFactory.generateCEventCodecsTuple[OUT])

    new RemoteGraphWithOutput[TupleFromCEvents[OUT]] {}
  }

  inline def withIO[OUT <: Tuple, IN <: Tuple : EventTupleUtils]
  (inline appName: String)(inline dependencies: IN)(inline graph: CEventsFromEvents[IN] => OUT)
  (using TupleUtils[TupleFromCEvents[OUT]], TupleCodec[TupleFromCEvents[OUT]], TupleCodec[TupleFromEvents[IN]]): RemoteGraphWithIO[IN, TupleFromCEvents[OUT]] = {
    macroCompiler.compileGraph(appName)(graph)

    given JsonValueCodec[OptionsFromEvents[IN]] =
      TupleCodecFactory.combineTupleCodecs(TupleCodecFactory.generateEventCodecsTuple[IN]).asInstanceOf[JsonValueCodec[OptionsFromEvents[IN]]]

    given JsonValueCodec[OptionsFromTuple[TupleFromCEvents[OUT]]] =
      TupleCodecFactory.combineTupleCodecs(TupleCodecFactory.generateCEventCodecsTuple[OUT])

    new RemoteGraphWithIO[IN, TupleFromCEvents[OUT]] {
      override val events: IN = dependencies
    }
  }
}
