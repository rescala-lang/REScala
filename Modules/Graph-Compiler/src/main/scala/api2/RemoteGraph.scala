package api2

import rescala.default.*
import com.github.plokhotnyuk.jsoniter_scala.core.*
import rescala.core.ReSource

trait RemoteGraph {
  protected var connector: Option[RemoteGraphConnector] = None

  def setConnector(c: RemoteGraphConnector): Unit = if connector.isEmpty then connector = Some(c)
}

trait RemoteGraphWithInput[IN <: Tuple: EventTupleUtils](using JsonValueCodec[OptionsFromEvents[IN]])
    extends RemoteGraph {
  val events: IN

  def startObserving(): Unit = {
    val dependencies = events.toList.map(_.asInstanceOf[ReSource.of[BundleState]])
    val grouped = Events.static(dependencies: _*) { t =>
      Some(summon[EventTupleUtils[IN]].staticAccesses(events, t))
    }

    grouped.observe(v => connector.get.write(v))
    ()
  }
}

trait RemoteGraphWithOutput[OUT <: Tuple: TupleUtils](using JsonValueCodec[OptionsFromTuple[OUT]]) extends RemoteGraph {
  def eventsFromListen(): EvtsFromTuple[OUT] = {
    val (evts, cbs) = summon[TupleUtils[OUT]].createEvtsWithCallbacks

    connector.get.read[OptionsFromTuple[OUT]] { v =>
      summon[TupleUtils[OUT]].processCallbacks(v, cbs)
    }

    evts
  }
}

trait RemoteGraphWithIO[IN <: Tuple: EventTupleUtils, OUT <: Tuple: TupleUtils](using
    JsonValueCodec[OptionsFromEvents[IN]],
    JsonValueCodec[OptionsFromTuple[OUT]]
) extends RemoteGraphWithInput[IN] with RemoteGraphWithOutput[OUT]
