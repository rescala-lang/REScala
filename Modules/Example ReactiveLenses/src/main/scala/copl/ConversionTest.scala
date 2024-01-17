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
    //signalTest()
    val temperatureConverter = toStringConverter()
    document.body.replaceChild(temperatureConverter.render, document.body.firstChild)
    ()
  }


  def signalTest() = {

    /**
    println("Init Vars")
    val a = LVar(2)
    val b = a.applyLens(new AddLens(10))
    val c = a.applyLens(new AddLens(-1))
    //val c = a.applyLens(new CharCountLens())
    println(a.now)
    println(b.now)
    println(c.now)

    println("Changing b")
    b.set(0)
    println(a.now)
    println(b.now)
    println(c.now)

    println("Changing c")
    //c.set("abc")
    c.set(5)
    println(a.now)
    println(b.now)
    println(c.now)
    **/

    //val g = LVar(5)
    //val h = g.applyLens(new CharCountLens())
    //println(g.now)
    //println(h.now)

    /**
    val x = LVar("kaesekuchen")
    val y = x.applyLens(new UpperCharLens() )
    println(x.now)
    println(y.now)
    y.set("APFELMUS")
    println(x.now)
    println(y.now)
    **/

  }

  def getOneWayConverter() = {

    val meterInput: TypedTag[Input] = input(placeholder := "Meters")
    val (meterEvent: Event[String], renderedMeter: Input) = RenderUtil.inputFieldHandler(meterInput, oninput, clear = false)

    val yardSignal: Signal[Option[Double]] = meterEvent.hold(init="Please enter a valid value for meters.").map { str => convertMeterToYard(str.toDoubleOption)}

    val yardParagraph: Signal[TypedTag[Paragraph]] = yardSignal.map { yard => p(if (yard.isEmpty) "Please enter a valid value for meters." else yard.get.toString)}

    div(p("One way conversion using Signal"), renderedMeter, yardParagraph.asModifier)
  }

//  def getTemperatureConverter() = {
//
//    val celsiusVar = LVar(0.0)
//    val kelvinVar = celsiusVar.applyLens(new AddLens(273.15))
//
//    val celsiusInput: TypedTag[Input] = input(value := celsiusVar.now)
//    val (celsiusEvent: Event[String], renderedCelsius: Input) = RenderUtil.inputFieldHandler(celsiusInput, oninput, clear = false)
//
//    val kelvinInput: TypedTag[Input] = input(value := kelvinVar.now)
//    val (kelvinEvent: Event[String], renderedKelvin: Input) = RenderUtil.inputFieldHandler(kelvinInput, oninput, clear = false)
//
//    celsiusEvent.observe{ str => celsiusVar.set(str.toDouble); renderedKelvin.value = kelvinVar.now.toString }
//    kelvinEvent.observe { str => kelvinVar.set(str.toDouble); renderedCelsius.value = celsiusVar.now.toString }
//
//    div(p("Unit Conversion with Lenses :D"), renderedCelsius, renderedKelvin)
//  }

  def testSignalLens() = {
    val leftVar = LVar(0.0)
    val summand = Var{3.0}
    val lensSig = Signal{ new AddLens(summand.value) }
    val rightVar = leftVar.applyLens(new BijectiveSigLens(lensSig))

    val leftInput: TypedTag[Input] = input(value := leftVar.now)
    val (leftEvent: Event[String], renderedLeft: Input) = RenderUtil.inputFieldHandler(leftInput, oninput, clear = false)

    val sigInput: TypedTag[Input] = input(value := summand.now)
    val (sigEvent: Event[String], renderedSig: Input) = RenderUtil.inputFieldHandler(sigInput, oninput, clear = false)

    val rightInput: TypedTag[Input] = input(value := rightVar.now)
    val (rightEvent: Event[String], renderedRight: Input) = RenderUtil.inputFieldHandler(rightInput, oninput, clear = false)

    leftEvent.observe { str => leftVar.set(str.toDouble);renderedSig.value = summand.now.toString; renderedRight.value = rightVar.now.toString }
    sigEvent.observe { str => summand.set(str.toDouble); renderedRight.value = rightVar.now.toString;  renderedLeft.value = leftVar.now.toString }
    rightEvent.observe { str => rightVar.set(str.toDouble); renderedSig.value = summand.now.toString; renderedLeft.value = leftVar.now.toString }

    div(p("Unit Conversion with Lenses :D"), renderedLeft, renderedSig, renderedRight)
  }

  def toStringConverter() = {

    val intVar = LVar(0)
    val strVar = intVar.applyLens(new CharCountLens())

    val intInput: TypedTag[Input] = input(value := intVar.now)
    val (intEvent: Event[String], renderedInt: Input) = RenderUtil.inputFieldHandler(intInput, oninput, clear = false)

    val strInput: TypedTag[Input] = input(value := strVar.now)
    val (strEvent: Event[String], renderedStr: Input) = RenderUtil.inputFieldHandler(strInput, oninput, clear = false)

    intEvent.observe { str => intVar.set(str.toInt); renderedStr.value = strVar.now }
    strEvent.observe { str => strVar.set(str); renderedInt.value = intVar.now.toString }

    div(p("Unit Conversion with Lenses :D"), renderedInt, renderedStr)
  }
//
//
  def convertMeterToYard(meter : Option[Double]): Option[Double] = {
    if(meter.isEmpty)
      Option.empty[Double]
    else
      Option[Double](meter.get * 0.9144)
  }

}
