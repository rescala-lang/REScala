package lofi_acl.example.travelplanner

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import lofi_acl.access.Filter
import lofi_acl.ardt.datatypes.LWW.filter
import lofi_acl.ardt.datatypes.ORMap.stringKeyORMapFilter
import rdts.base.{Bottom, Lattice}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.dotted.{HasDots, Obrem}

type Title = String
given Bottom[Title] = Bottom.provide("")
type UniqueId = String

case class TravelPlan(
    title: LastWriterWins[Title],
    bucketList: Obrem[ObserveRemoveMap[UniqueId, LastWriterWins[String]]],
    expenses: Obrem[ObserveRemoveMap[UniqueId, Expense]]
) derives Lattice, Bottom, Filter

case class Expense(
    // time: LastWriterWins[LocalDateTime], // For what do we need time?
    description: LastWriterWins[Option[String]],
    amount: LastWriterWins[Option[Float]],
    comments: ObserveRemoveMap[String, LastWriterWins[String]]
) derives Lattice, HasDots, Bottom, Filter

object TravelPlan {
  val empty: TravelPlan = Bottom[TravelPlan].empty

  import lofi_acl.sync.JsoniterCodecs.uidKeyCodec
  given jsonCodec: JsonValueCodec[TravelPlan] = JsonCodecMaker.make[TravelPlan]
}

object Expense {
  val empty: Expense = Bottom[Expense].empty
}
