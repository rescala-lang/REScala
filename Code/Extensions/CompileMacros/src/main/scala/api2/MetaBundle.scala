package rescala.api2

import clangast.WithContext
import clangast.decl.CFunctionDecl
import clangast.types.CType
import compiler.*
import compiler.context.ReactiveTC
import compiler.debug.Debug
import compiler.ExtensionMethods.*
import rescala.core.ReName
import rescala.macros.MacroAccess
import rescala.operator.RExceptions
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.github.plokhotnyuk.jsoniter_scala.core.*

import scala.quoted.*
import scala.collection.mutable

transparent trait CompilerReactiveMarker

trait MetaBundle extends rescala.core.Core {
  bundle =>

  type MC <: ReactiveMacroCompiler
  val macroCompiler: MC

  // We flatten the scheduler and the API here, so we do not need a separate state
  override type State[A] = A

  // this is a mostly minimal version of the transaction
  case object Tx extends Transaction {
    private[rescala] def access(reactive: ReSource): reactive.Value = reactive.state
    def initializer: Initializer = new Initializer:
      override protected[this] def makeDerivedStructState[V](initialValue: V): State[V] = initialValue
      override protected[this] def initialize(
          reactive: Derived,
          incoming: Set[ReSource],
          needsReevaluation: Boolean
      ): Unit = ()
  }
  given (using ReName): CreationTicket = CreationTicket(new ScopeSearch(Left(Tx)), summon)

  // type parameter less superclass of our main abstraction,
  // this just makes some of the type annotations later simpler
  trait MReSource extends ReSource {
    def triggers: Set[MReSource]
    // We never read a any resource, so they have a unit value
    final type Value = Unit
    override protected[rescala] def commit(base: Value): Value = base
  }

  // the core reactive abstraction for this API. The name is a REScala thing that would not be strictly necessary,
  // but is fun to have
  // note, that this is generally quite close to REScala, which also does not really distinguish between signals and events,
  // but instead just has this single type which derives its semantics from the opaque `fun`, and the set of triggers which define when this activates
  case class MetaReactive[T, RT <: RType](
      triggers: Set[MReSource],
      name: ReName,
      kind: RT,
      fun: StaticTicket => T
  ) extends MReSource
      // the following two supertypes make this available as something that is accessed by the macro
      with MacroAccess[T, MReSource] with ReadAs[T] with CompilerReactiveMarker {
    override protected[rescala] def state: State[Unit] = ()
    override def resource: MReSource                   = this
    // can not implement, we lie about having a T, but there is also no execution logic, so this can never get called
    override def read(v: Unit): T = ???
  }

  // enum to introspect the type of meta reactive we meant to construct
  enum RType:
    case Signal, Event, Fold, CInterop

  // adapt the more general macro to our specific use case
  inline def getDeps[T](inline expr: T): (List[MReSource], StaticTicket => T) =
    rescala.macros.getDependencies[T, MReSource, StaticTicket, true](expr)

  inline def Signal[T](inline expr: T)(using ct: CreationTicket): MetaReactive[T, RType.Signal.type] = {
//    val (dependencies, fun) = getDeps(expr)
//    MetaReactive(
//      dependencies.toSet,
//      ct.rename,
//      RType.Signal,
//      fun
//    )
    ???
  }

  inline def Event[T](inline expr: Option[T])(using ct: CreationTicket): MetaReactive[Option[T], RType.Event.type] = {
//    val (dependencies, fun) = getDeps(expr)
//    MetaReactive(
//      dependencies.toSet,
//      ct.rename,
//      RType.Event,
//      fun
//    )
    ???
  }

  extension [T](mr: MetaReactive[Option[T], RType.Event.type])
    inline def fold[S](inline init: S)(inline f: (S, T) => S)(using
        ct: CreationTicket
    ): MetaReactive[S, RType.Fold.type] = {
//      val (deps, fun) = getDeps {
//        f(init, mr.value.get)
//      }
//      MetaReactive(
//        deps.toSet,
//        ct.rename,
//        RType.Fold,
//        fun
//      )
      ???
    }

  extension[T] (inline ev: MetaReactive[Option[T], RType.Event.type] ) {
    inline def map[R](inline f: T => R): MetaReactive[Option[R], RType.Event.type] = Event {
      ev.value.map(f)
    }

    inline def observe(inline f: T => Unit): MetaReactive[Option[Int], RType.Event.type] = Event {
      ev.value.foreach(f)
      Option.empty[Int]
    }
  }

  extension[T] (inline sig: MetaReactive[T, RType.Signal.type] | MetaReactive[T, RType.Fold.type] ) {
    inline def observeChange(inline f: T => Unit): MetaReactive[Option[Int], RType.Event.type] = Event {
      f(sig.value)
      Option.empty[Int]
    }
  }

}

object StandardBundle extends MetaBundle {
  type MC = ReactiveMacroCompiler.type
  override val macroCompiler: MC = ReactiveMacroCompiler

  type MetaReactivesFromEvents[T <: Tuple] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case rescala.default.Event[t] *: ts => MetaReactive[Option[t], RType.Event.type] *: MetaReactivesFromEvents[ts]

  type TupleFromMetaReactives[T <: Tuple] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case MetaReactive[Option[t], RType.Event.type] *: ts => t *: TupleFromMetaReactives[ts]

  object CompileGraph {
    export macroCompiler.compileGraph as isolated

    inline def withInput[IN <: Tuple : EventTupleUtils]
    (inline appName: String)(inline dependencies: IN)(inline graph: MetaReactivesFromEvents[IN] => Unit): RemoteGraphWithInput[IN] = {

      macroCompiler.compileGraph(appName)(graph)

      given JsonValueCodec[OptionsFromEvents[IN]] = TupleCodecFactory.generateEventCodecs[IN]

      new RemoteGraphWithInput[IN] {
        override val events: IN = dependencies
      }
    }

    inline def withOutput[OUT <: Tuple]
    (inline appName: String)(inline graph: OUT)
    (using TupleUtils[TupleFromMetaReactives[OUT]], TupleCodec[TupleFromMetaReactives[OUT]]): RemoteGraphWithOutput[TupleFromMetaReactives[OUT]] = {

      macroCompiler.compileGraph(appName)(graph)

      given JsonValueCodec[OptionsFromTuple[TupleFromMetaReactives[OUT]]] =
        TupleCodecFactory.combineTupleCodecs(TupleCodecFactory.generateMetaReactiveCodecsTuple[OUT])

      new RemoteGraphWithOutput[TupleFromMetaReactives[OUT]] {}
    }

    inline def withIO[OUT <: Tuple, IN <: Tuple : EventTupleUtils]
    (inline appName: String)(inline dependencies: IN)(inline graph: MetaReactivesFromEvents[IN] => OUT)
    (using TupleUtils[TupleFromMetaReactives[OUT]], TupleCodec[TupleFromMetaReactives[OUT]]): RemoteGraphWithIO[IN, TupleFromMetaReactives[OUT]] = {
      macroCompiler.compileGraph(appName)(graph)

      given JsonValueCodec[OptionsFromEvents[IN]] = TupleCodecFactory.generateEventCodecs[IN]

      given JsonValueCodec[OptionsFromTuple[TupleFromMetaReactives[OUT]]] =
        TupleCodecFactory.combineTupleCodecs(TupleCodecFactory.generateMetaReactiveCodecsTuple[OUT])

      new RemoteGraphWithIO[IN, TupleFromMetaReactives[OUT]] {
        override val events: IN = dependencies
      }
    }
  }
}

object MetaBundleExample {
  import StandardBundle.*

  @main
  def run(): Unit = {

    // did not bother to include sources, so just start with constant signals
    CompileGraph.isolated("metaBundleTest") {
      val source = Signal(5)

      val inc = 1

      val derived = Signal {
        source.value + inc
      }

      val arraySignal = Signal { Array(derived.value) }

      arraySignal.observeChange(a => println(a))

      val esource = Event { Some("Hi!") }

      val emapped = esource.map(str => str)

      val arrayEvent = emapped.map { str => Array(str) }

      val filtered = Event { emapped.value.filter(_.length == 3) }

      def someTuple(a: String, b: String): Option[(String, String)] = Some((a, b))

      val zipped = Event {
        (emapped.value, filtered.value) match
          case (Some(left), Some(right)) => someTuple(left, right)
          case _                         => Option.empty[(String, String)]
      }

      zipped.observe(t => println(t))

      val snapshotLike = Event { esource.value.map(_ => derived.value) }

      val snapshotLike2 = esource.map(_ => derived.value)

      val foldResult = esource.fold(mutable.Set[String]()) { (acc, next) =>
        acc.add(next)
        acc
      }

      foldResult.observeChange(s => println(s))
    }

    val source = Signal(5)

    val derived = Signal {
      source.value + 1
    }

    // val nested = Signal { Signal ( 4 ) }
    // val flattened = Signal { nested.value.value }

    // kinda weird „constant“ event here, that makes not much sense, but provides the correct type for the rest
    val esource = Event { Some("Hi!") }

    // note that .value on events returns an option and these event expressions expect an option
    // for expressions such as this, which has only a single trigger, it is guaranteed that the option is always defined,
    // but for multiple triggers any of them may be None
    val emapped = Event { esource.value.map(_.toUpperCase()) }

    // filtering is not a special thing, REScala just interprets the returned option as “do not continue activation”.
    val filtered = Event { emapped.value.filter(_.contains("i")) }

    // this is the basic zip operation on events
    val zipped = Event {
      (emapped.value, filtered.value) match
        case (Some(left), Some(right)) => Some((left, right))
        case _                         => None
    }

    // this is how snapshot is implemented in REScala
    // we just access the derived signal every time the esource activates
    // when derived activates without esource, then the expression returns None, which will cause propagation to stop
    val snapshotLike = Event { esource.value.map(_ => derived.value) }

    val snapshotLike2 = esource.map(_ => derived.value)

    // fold is kinda the only special operation that is normally needed
    val foldResult = esource.fold("") { (acc, next) => acc + next }

    // folds can make use of other reactives
    val foldResultWithMoreStuff = esource.fold("") { (acc, next) => acc + next + derived.value }

    val all = Signal { (foldResult.value, snapshotLike.value, filtered.value, foldResultWithMoreStuff, zipped.value) }

    println(all)

    //// for testing, it would be cool to have a
    // def CinteropSource[T](v: T) = MetaReactive(Set.empty, ReName("c source"), RType.CInterop, _ => v)
    //
    //// and then do something like
    //
    // val source1 = CinteropSource(1)
    //
    // val csig = Signal { source1.value * 10 }

    // does not exists, but something like
    //   val compiled = csig.compile
    //   compiled.addObserver(csig, i => println(i))
    //   compiled.set(source1, 10)
    // and have that print 100

  }

}
