package copl

import org.scalajs.dom.html.{Input, Paragraph, Select}
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
//    signalTest()
    val temperatureConverter = basicCalculator()
    document.body.replaceChild(temperatureConverter.render, document.body.firstChild)
    ()
  }

  //TODO: Non-determenistic lens behaviour problematic due to simplified propagation
  //TODO: Exceptions "disconnect" fields


  def signalTest() = {

    val evA: toposort.Evt[Int] = Evt[Int]()
    val evB: toposort.Evt[Int] = Evt[Int]()
    val evC: toposort.Evt[Int] = Evt[Int]()
    val a = LVar(2)
    val b = a.applyLens(new AddLens(10))
    val c = a.applyLens(new AddLens(-1))
    a.fire(evA)
    b.fire(evB)
    c.fire(evC)
    a.getEvent().observe{value => println("Value of a changed to " + value)}
    b.getEvent().observe{value => println("Value of b changed to " + value)}
    c.getEvent().observe{value => println("Value of c changed to " + value)}

    println("Init Vars")
    println(a.now)
    println(b.now)
    println(c.now)

    println("Changing a")
    evA.fire(0)
    println(a.now)
    println(b.now)
    println(c.now)

    println("Changing b")
    evB.fire(0)
    println(a.now)
    println(b.now)
    println(c.now)

    println("Changing c")
    evC.fire(5)
    println(a.now)
    println(b.now)
    println(c.now)

  }

  def basicCalculator() = {
    val leftVar = LVar(0.0)

    //Handle operation dropdown
    val operationInput: TypedTag[Select] = select(option("ADD"), option("SUB"))
    val (operationEvent: Event[String], renderedOperation: Select) = RenderUtil.dropDownHandler(operationInput, oninput, clear = false)
    val operationSignal: Signal[String] = operationEvent.hold(init = renderedOperation.value)

    //Handle value input
    val sigInput: TypedTag[Input] = input(value := 0.0)
    val (sigEvent: Event[String], renderedSig: Input) = RenderUtil.inputFieldHandler(sigInput, oninput, clear = false)
    val valueSignal: Signal[Double] = sigEvent.hold(init = renderedSig.value).map { (str: String) => (str.toDouble: Double) }

    val lens : BijectiveSigLens[Double, Double] = new BijectiveSigLens[Double, Double](Signal{
      if (operationSignal.value == "ADD") {
        new AddLens[Double](valueSignal.value)
      } else if (operationSignal.value == "SUB") {
        new AddLens[Double](valueSignal.value).inverse
      } else {
        new NeutralLens[Double]
      }
    })

    val rightVar = leftVar.applyLens(lens)

    val leftInput: TypedTag[Input] = input(value := leftVar.now)
    val (leftEvent: Event[String], renderedLeft: Input) = RenderUtil.inputFieldHandler(leftInput, oninput, clear = false)

    val rightInput: TypedTag[Input] = input(value := rightVar.now)
    val (rightEvent: Event[String], renderedRight: Input) = RenderUtil.inputFieldHandler(rightInput, oninput, clear = false)

    leftVar.fire(leftEvent.map(str => str.toDouble))
    rightVar.fire(rightEvent.map(str => str.toDouble))

    leftVar.observe { value => ; renderedLeft.value = value.toString }
    rightVar.observe { value => ; renderedRight.value = value.toString }

    div(p("A simple Calculator"), renderedLeft, renderedOperation, renderedSig, renderedRight)
  }

  def getOneWayConverter() = {

    val meterInput: TypedTag[Input] = input(placeholder := "Meters")
    val (meterEvent: Event[String], renderedMeter: Input) = RenderUtil.inputFieldHandler(meterInput, oninput, clear = false)

    val yardSignal: Signal[Option[Double]] = meterEvent.hold(init="Please enter a valid value for meters.").map { str => convertMeterToYard(str.toDoubleOption)}

    val yardParagraph: Signal[TypedTag[Paragraph]] = yardSignal.map { yard => p(if (yard.isEmpty) "Please enter a valid value for meters." else yard.get.toString)}

    div(p("One way conversion using Signal"), renderedMeter, yardParagraph.asModifier)
  }

  def getTemperatureConverter() = {

    val celsiusVar = LVar(0.0)
    val kelvinVar = celsiusVar.applyLens(new AddLens(273.15).compose(new AddLens(273.15).inverse))

    val celsiusInput: TypedTag[Input] = input(value := celsiusVar.now)
    val (celsiusEvent: Event[String], renderedCelsius: Input) = RenderUtil.inputFieldHandler(celsiusInput, oninput, clear = false)

    val kelvinInput: TypedTag[Input] = input(value := kelvinVar.now)
    val (kelvinEvent: Event[String], renderedKelvin: Input) = RenderUtil.inputFieldHandler(kelvinInput, oninput, clear = false)

    celsiusVar.fire(celsiusEvent.map {str => if (str.toDoubleOption.isDefined) str.toDouble else 0.0 })
    kelvinVar.fire(kelvinEvent.map {str => if (str.toDoubleOption.isDefined) str.toDouble else 0.0 })

    kelvinVar.observe{value => ;renderedKelvin.value = value.toString}
    celsiusVar.observe{value => ;renderedCelsius.value = value.toString}

    div(p("Unit Conversion with Lenses :D"), renderedCelsius, renderedKelvin)
  }

  def testSignalLens() = {
    val leftVar = LVar(0.0)

    val sigInput: TypedTag[Input] = input(value := 3.0)
    val (sigEvent: Event[String], renderedSig: Input) = RenderUtil.inputFieldHandler(sigInput, oninput, clear = false)

    val lensSig = Signal{ new AddLens(sigEvent.hold(init = renderedSig.value).map {(str : String) => (str.toDouble : Double)}.value).inverse}

    val rightVar = leftVar.applyLens(new BijectiveSigLens(lensSig))

    val leftInput: TypedTag[Input] = input(value := leftVar.now)
    val (leftEvent: Event[String], renderedLeft: Input) = RenderUtil.inputFieldHandler(leftInput, oninput, clear = false)

    val rightInput: TypedTag[Input] = input(value := rightVar.now)
    val (rightEvent: Event[String], renderedRight: Input) = RenderUtil.inputFieldHandler(rightInput, oninput, clear = false)

    leftVar.fire(leftEvent.map(str => str.toDouble))
    rightVar.fire(rightEvent.map(str => str.toDouble))

    leftVar.observe{value =>; renderedLeft.value = value.toString}
    rightVar.observe{value =>; renderedRight.value = value.toString}

    div(p("Unit Conversion with Lenses :D"), renderedLeft, renderedSig, renderedRight)
  }


  def toStringConverter() = {

    val intVar = LVar(0)
    val strVar = intVar.applyLens(new NonDeterministicCharCountLens())

    val intInput: TypedTag[Input] = input(value := intVar.now)
    val (intEvent: Event[String], renderedInt: Input) = RenderUtil.inputFieldHandler(intInput, oninput, clear = false)

    val strInput: TypedTag[Input] = input(value := strVar.now)
    val (strEvent: Event[String], renderedStr: Input) = RenderUtil.inputFieldHandler(strInput, oninput, clear = false)

    intVar.fire(intEvent.map{_.toInt})
    strVar.fire(strEvent)

    intVar.observe{value => ; renderedInt.value = value.toString}
    strVar.observe{value => ; renderedStr.value = value}

    div(p("Unit Conversion with Lenses :D"), renderedInt, renderedStr)
  }

  def convertMeterToYard(meter : Option[Double]): Option[Double] = {
    if(meter.isEmpty)
      Option.empty[Double]
    else
      Option[Double](meter.get * 0.9144)
  }

}
