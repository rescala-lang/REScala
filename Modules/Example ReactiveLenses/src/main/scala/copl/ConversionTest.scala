package copl

import org.scalajs.dom.html.{Input, Select}
import org.scalajs.dom.document
import rescala.interfaces.toposort
import rescala.interfaces.toposort.*
import scalatags.JsDom
import scalatags.JsDom.all.*
import scalatags.JsDom.TypedTag

import scala.scalajs.js.annotation.JSExportTopLevel

object ConversionTest {

  @JSExportTopLevel("UnitConversion")
  def run(): Unit = main(Array.empty[String])

  def main(args: Array[String]): Unit = {
//    signalTest()
    val temperatureConverter = unitConverter()
    document.body.replaceChild(temperatureConverter.render, document.body.firstChild)
    ()
  }


  enum TempConversion(val lens: BijectiveLens[Double, Double]):
    case C extends TempConversion(new NeutralLens)
    case K extends TempConversion(new AddLens(274.15))
    case L extends TempConversion(new AddLens(253))
    case F extends TempConversion(new MulLens(1.8).compose(new AddLens(32.0)))
  end TempConversion

  def conversionLens(from : TempConversion, to : TempConversion): BijectiveLens[Double, Double] = from.lens.inverse.compose(to.lens)

  def unitConverter() = {

    val leftUnitInput: TypedTag[Select] = select(TempConversion.values.map{ unit => option(unit.toString) })
    val (leftUnitEvent: Event[String], renderedLeftUnit: Select) = RenderUtil.dropDownHandler(leftUnitInput, oninput, clear = false)
    val leftUnitSignal: Signal[TempConversion] = leftUnitEvent.hold(init = renderedLeftUnit.value).map{TempConversion.valueOf(_)}

    val rightUnitInput: TypedTag[Select] = select(TempConversion.values.map { unit => option(unit.toString) })
    val (rightUnitEvent: Event[String], renderedRightUnit: Select) = RenderUtil.dropDownHandler(rightUnitInput, oninput, clear = false)
    val rightUnitSignal: Signal[TempConversion] = rightUnitEvent.hold(init = renderedRightUnit.value).map{TempConversion.valueOf(_)}

    val leftVar = LVar(0.0)
    val rightVar = leftVar.applyLens(LensSig(Signal{conversionLens(leftUnitSignal.value, rightUnitSignal.value)}))

    val leftValueInput: TypedTag[Input] = input(value := leftVar.now)
    val (leftValueEvent: Event[String], renderedLeftValue: Input) = RenderUtil.inputFieldHandler(leftValueInput, oninput, clear = false)

    val rightValueInput: TypedTag[Input] = input(value := rightVar.now)
    val (rightValueEvent: Event[String], renderedRightValue: Input) = RenderUtil.inputFieldHandler(rightValueInput, oninput, clear = false)

    leftVar.fire(leftValueEvent.map{toDoubleOr0(_)})
    rightVar.fire(rightValueEvent.map{toDoubleOr0(_)})

    leftVar.observe{ value => RenderUtil.setInputDisplay(renderedLeftValue, value.toString) }
    rightVar.observe{ value => RenderUtil.setInputDisplay(renderedRightValue, value.toString) }

    div(p("Unit Conversion with Lenses"), renderedLeftUnit, renderedRightUnit, br , renderedLeftValue, renderedRightValue)

  }

  def toDoubleOr0(str : String): Double = {
    try {
      {str.toDouble}
    } catch {
      case _ => {0.0}
    }
  }

}
