package copl

import org.scalajs.dom.html.{Input, Paragraph, Select}
import org.scalajs.dom.document
import rescala.extra.Tags
import rescala.interfaces.toposort
import rescala.interfaces.toposort.*
import scalatags.JsDom
import scalatags.JsDom.all.*
import scalatags.JsDom.TypedTag

import scala.annotation.switch
import scala.scalajs.js.annotation.JSExportTopLevel

object TopoTags extends Tags[rescala.interfaces.toposort.type](rescala.interfaces.toposort, true)
import TopoTags.*

object ConversionTest {

  @JSExportTopLevel("UnitConversion")
  def run(): Unit = main(Array.empty[String])

  def main(args: Array[String]): Unit = {
//    signalTest()
    val temperatureConverter = unitConverter()
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
    val operationInput: TypedTag[Select] = select(option("+"), option("-"), option("*"), option("/"))
    val (operationEvent: Event[String], renderedOperation: Select) = RenderUtil.dropDownHandler(operationInput, oninput, clear = false)
    val operationSignal: Signal[String] = operationEvent.hold(init = renderedOperation.value)

    //Handle value input
    val valueInput: TypedTag[Input] = input(value := 0.0)
    val (valueEvent: Event[String], renderedValue: Input) = RenderUtil.inputFieldHandler(valueInput, oninput, clear = false)
    val valueSignal: Signal[Double] = valueEvent.hold(init = renderedValue.value).map{toDoubleOr0(_)}

    val lens : BijectiveSigLens[Double, Double] = new BijectiveSigLens[Double, Double](Signal{
        (operationSignal.value: @switch) match {
          case "+" => new AddLens(valueSignal.value)
          case "-" => new AddLens(valueSignal.value).inverse
          case "*" => new MulLens(valueSignal.value)
          case "/" => new MulLens(valueSignal.value).inverse
          case _ => new NeutralLens
        }
    })

    val rightVar = leftVar.applyLens(lens)

    val leftInput: TypedTag[Input] = input(value := leftVar.now)
    val (leftEvent: Event[String], renderedLeft: Input) = RenderUtil.inputFieldHandler(leftInput, oninput, clear = false)

    val rightInput: TypedTag[Input] = input(value := rightVar.now)
    val (rightEvent: Event[String], renderedRight: Input) = RenderUtil.inputFieldHandler(rightInput, oninput, clear = false)

    leftVar.fire(leftEvent.map{toDoubleOr0(_)})
    rightVar.fire(rightEvent.map{toDoubleOr0(_)})

    //TODO This is ugly
    leftVar.observe { value => ; renderedLeft.value = value.toString }
    rightVar.observe { value => ; renderedRight.value = value.toString }

    div(p("A simple Calculator"), renderedLeft, renderedOperation, renderedValue, " = ", renderedRight)
  }

  def getOneWayConverter() = {

    val meterInput: TypedTag[Input] = input(placeholder := "Meters")
    val (meterEvent: Event[String], renderedMeter: Input) = RenderUtil.inputFieldHandler(meterInput, oninput, clear = false)

    val yardSignal: Signal[Option[Double]] = meterEvent.hold(init="Please enter a valid value for meters.").map { str => convertMeterToYard(str.toDoubleOption)}

    val yardParagraph: Signal[TypedTag[Paragraph]] = yardSignal.map { yard => p(if (yard.isEmpty) "Please enter a valid value for meters." else yard.get.toString)}

    div(p("One way conversion using Signal"), renderedMeter, yardParagraph.asModifier)
  }

  def unitConverter() = {

    val leftUnitInput: TypedTag[Select] = select(option("m"), option("km"))
    val (leftUnitEvent: Event[String], renderedLeftUnit: Select) = RenderUtil.dropDownHandler(leftUnitInput, oninput, clear = false)
    //val leftUnitSignal: Signal[String] = leftUnitEvent.hold(init = renderedLeftUnit.value)

    val rightUnitInput: TypedTag[Select] = select(option("m"), option("km"))
    val (rightUnitEvent: Event[String], renderedRightUnit: Select) = RenderUtil.dropDownHandler(rightUnitInput, oninput, clear = false)
    //val rightUnitSignal: Signal[String] = rightUnitEvent.hold(init = renderedRightUnit.value)

    val leftValueVar = LVar(1.0)
    val leftValueInput: TypedTag[Input] = input(value := leftValueVar.now)
    val (leftValueEvent: Event[String], renderedLeftValue: Input) = RenderUtil.inputFieldHandler(leftValueInput, oninput, clear = false)

    val rightValueVar = LVar(1.0)
    val rightValueInput: TypedTag[Input] = input(value := rightValueVar.now)
    val (rightValueEvent: Event[String], renderedRightValue: Input) = RenderUtil.inputFieldHandler(rightValueInput, oninput, clear = false)

    div(p("Unit Conversion with Lenses"), renderedLeftUnit, renderedRightUnit, br, renderedLeftValue, renderedRightValue)
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

    leftVar.fire(leftEvent.map{toDoubleOr0(_)})
    rightVar.fire(rightEvent.map{toDoubleOr0(_)})

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

  def toDoubleOr0(str : String): Double = {
    try {
      {str.toDouble}
    } catch {
      case _ => {0.0}
    }
  }

}
