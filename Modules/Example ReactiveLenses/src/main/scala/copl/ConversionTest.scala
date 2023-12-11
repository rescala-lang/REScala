package copl

import org.scalajs.dom.html.{Input, Paragraph}
import org.scalajs.dom.document
import rescala.extra.Tags
import rescala.interfaces.toposort
import rescala.interfaces.toposort.*
import scalatags.JsDom
import scalatags.JsDom.all.*
import scalatags.JsDom.TypedTag

import scala.scalajs.js.annotation.JSExportTopLevel

object TopoTags extends Tags[rescala.interfaces.toposort.type](rescala.interfaces.toposort, true)
import TopoTags.*

object ConversionTest {

  @JSExportTopLevel("UnitConversion")
  def run(): Unit = main(Array.empty[String])

  def main(args: Array[String]): Unit = {
    signalTest()
    //val oneWayConverter = getOneWayConverter()
    //document.body.replaceChild(oneWayConverter.render, document.body.firstChild)
    //()
  }

  def signalTest() = {

    val a = LVar(2)
//    val b = Var(3)
//
//    println(a.now)
//    println(b.now)
//
//    val c: toposort.Signal[Int] = Signal { a.value + b.value } // { a() + b() }
//    println(a.now)
//    println(b.now)
//    println(c.now)
//
//    a.set(1)
//    println(a.now)
//    println(b.now)
//    println(c.now)

    println("Starting Lens Tests")
    val d = a.applyLens(new AddLens(10))
    println(a.now)
    println(d.now)
    d.set(20)
    println(a.now)
//    println(b.now)
//    println(c.now)
    println(d.now)

  }

  def getOneWayConverter() = {

    val meterInput: TypedTag[Input] = input(placeholder := "Meters")
    val (meterEvent: Event[String], renderedMeter: Input) = RenderUtil.inputFieldHandler(meterInput, oninput, clear = false)

    val yardSignal: Signal[Option[Double]] = meterEvent.hold(init="Please enter a valid value for meters.").map { str => convertMeterToYard(str.toDoubleOption)}

    val yardParagraph: Signal[TypedTag[Paragraph]] = yardSignal.map { yard => p(if (yard.isEmpty) "Please enter a valid value for meters." else yard.get.toString)}

    div(p("One way conversion using Signal"), renderedMeter, yardParagraph.asModifier)
  }

  def convertMeterToYard(meter : Option[Double]): Option[Double] = {
    if(meter.isEmpty)
      Option.empty[Double]
    else
      Option[Double](meter.get * 0.9144)
  }

}
