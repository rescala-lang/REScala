package tests.rescala.dynamic.conversions

import rescala.core.CreationTicket
import tests.rescala.util.RETests


class Change extends RETests {

  allEngines("create changes during reevaluation"){ engine => import engine._
    val v = Var(1)
    val mapped = v.map(_ + 0)

    val sm = Signal { mapped.change.apply() }
    val sd = dynamic() {t => t.depend(mapped.change(CreationTicket.fromTicketDImplicit(t, implicitly))) }


    //intercept[NoSuchElementException](sm.now)
    assert(sm.now.isEmpty)
    assert(sd.now.isEmpty)

    v.set(2)

    assert(sm.now.get.pair == 1 -> 2)
    assert(sd.now.get.pair == 1 -> 2)

    v.set(3)

    assert(sm.now.get.pair == 2 -> 3)
    assert(sd.now.get.pair == 2 -> 3)

  }


}
