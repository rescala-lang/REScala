package api2

import scala.annotation.unused
import scala.collection.mutable

object MetaBundleExample extends App {
  CompileGraph.isolated("metaBundleTest") {
    val source = CSignal.source(5)

    val inc = 1

    val derived = CSignal {
      source.value + inc
    }

    val arraySignal = CSignal {
      Array(derived.value)
    }

    arraySignal.observeChange(a => println(a))

    val esource = CEvent.source[String]

    val emapped = esource.map(str => str)

    @unused
    val arrayEvent = emapped.map { str => Array(str) }

    val filtered = CEvent {
      emapped.value.filter(_.length == 3)
    }

    def someTuple(a: String, b: String): Option[(String, String)] = Some((a, b))

    val zipped = CEvent {
      (emapped.value, filtered.value) match
        case (Some(left), Some(right)) => someTuple(left, right)
        case _                         => Option.empty[(String, String)]
    }

    zipped.observe(t => println(t))

    @unused
    val snapshotLike = CEvent {
      esource.value.map(_ => derived.value)
    }

    @unused
    val snapshotLike2 = esource.map(_ => derived.value)

    val foldResult = esource.fold(mutable.Set[String]()) { (acc, next) =>
      acc.add(next)
      acc
    }

    foldResult.observeChange(s => println(s))
  }
}
