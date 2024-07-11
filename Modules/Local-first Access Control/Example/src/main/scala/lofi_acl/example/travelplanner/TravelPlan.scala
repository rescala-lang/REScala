package lofi_acl.example.travelplanner

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import lofi_acl.access.Filter
import lofi_acl.ardt.datatypes.LWW.filter
import lofi_acl.ardt.datatypes.ORMap.{observeRemoveMapEntryFilter, stringKeyORMapFilter}
import lofi_acl.example.travelplanner.Expense.Description
import lofi_acl.example.travelplanner.TravelPlan.{Title, UniqueId}
import rdts.base.{Bottom, Lattice}
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.datatypes.contextual.ObserveRemoveMap.Entry
import rdts.dotted.{HasDots, Obrem}

case class TravelPlan(
    title: LastWriterWins[Title],
    bucketList: Obrem[ObserveRemoveMap[UniqueId, Entry[LastWriterWins[String]]]],
    expenses: Obrem[ObserveRemoveMap[UniqueId, Entry[Expense]]]
) derives Lattice, Bottom, Filter

case class Expense(
    description: LastWriterWins[Option[Description]],
    amount: LastWriterWins[Option[Float]],
    comment: LastWriterWins[Option[String]],
) derives Lattice, HasDots, Bottom, Filter

object TravelPlan {
  type Title = String
  given Bottom[Title] = Bottom.provide("")
  type UniqueId = String
  val empty: TravelPlan = Bottom[TravelPlan].empty

  import lofi_acl.sync.JsoniterCodecs.uidKeyCodec
  given jsonCodec: JsonValueCodec[TravelPlan] = JsonCodecMaker.make[TravelPlan]
}

object Expense {
  type Description = String
  given Bottom[Description] = Bottom.provide("")
  val empty: Expense        = Bottom[Expense].empty
}
