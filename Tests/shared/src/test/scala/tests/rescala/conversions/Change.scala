package tests.rescala.conversions

import rescala.core.{CreationTicket, Pulse}
import rescala.reactives.RExceptions.EmptySignalControlThrowable
import tests.rescala.util.RETests

import scala.collection.LinearSeq


class Change extends RETests {


  /* change */
  allEngines("change is Not Triggered On Creation") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e = s1.change
    e += { x => test += 1 }

    assert(test == 0)
  }

  allEngines("change is Triggered When The Signal Changes") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e = s1.change
    e += { x => test += 1 }

    v1 set 2
    assert(test == 1)
    v1 set 3
    assert(test == 2)
  }

  allEngines("change the Value Of The Event Reflects The Change In The Signal") { engine => import engine._
    var test = (0, 0)
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e = s1.change
    e += { x => test = x.pair }

    v1 set 2
    assert(test === ((2, 3)))
    v1 set 3
    assert(test === ((3, 4)))
  }

  /* changed */
  allEngines("changed is Not Triggered On Creation") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e: Event[Int] = s1.changed
    e += ((x: Int) => {test += 1})

    assert(test == 0)
  }

  allEngines("changed is Triggered When The Signal Changes") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e: Event[Int] = s1.changed
    e += ((x: Int) => {test += 1})

    v1 set 2
    assert(test == 1)
    v1 set 3
    assert(test == 2)
  }

  allEngines("changed the Value Of The Event Reflects The Change In The Signal") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e: Event[Int] = s1.changed
    e += ((x: Int) => {test = x})

    v1 set 2
    assert(test == 3)
    v1 set 3
    assert(test == 4)
  }

  /* changedTo */
  allEngines("changed To is Not Triggered On Creation") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e: Event[Unit] = s1.changedTo(1)
    e += ((x: Unit) => {test += 1})

    assert(test == 0)
  }

  allEngines("changed To is Triggered When The Signal Has The Given Value") { engine => import engine._
    var test = 0
    val v1 = Var(1)
    val s1 = v1.map {_ + 1}
    val e: Event[Unit] = s1.changedTo(3)
    e += ((x: Unit) => {test += 1})

    v1 set 2
    assert(test == 1)
    v1 set 3
    assert(test == 1)
  }

  allEngines("folding changing and emptyness") { engine => import engine._
    val v1 = Var.empty[String]
    val v2 = Var.empty[String]

    val e1 = v1.changed.map(x => ("constant", x))
    val e2 = v2.change.map(_.pair).recover{case _ => Some("failed" -> "change")}

    val ored: Event[(String, String)] = e1 || e2

    val log = ored.list()

    assert(log.now === Nil)

    v1.set("one")
    assert(log.now === List("constant" -> "one"))

    v2.set("two")
    assert(log.now === List("constant" -> "one"))

    v2.set("three")
    assert(log.now === List("two" -> "three", "constant" -> "one"))


    update(v1 -> "four a", v2 -> "four b")

    assert(log.now === List("constant" -> "four a", "two" -> "three", "constant" -> "one"))

    transaction(v1, v2) { turn =>
      v1.admitPulse(Pulse.Exceptional(EmptySignalControlThrowable))(turn)
      v2.admit("five b")(turn)
    }

    assert(log.now === List("four b" -> "five b", "constant" -> "four a", "two" -> "three", "constant" -> "one"))

  }

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
