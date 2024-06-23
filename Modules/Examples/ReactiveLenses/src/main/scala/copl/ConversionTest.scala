package copl

import org.scalajs.dom.document
import org.scalajs.dom.html.{Input, Select}
import reactives.default.*
import reactives.extra.lenses.*
import scalatags.JsDom
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.*

import scala.scalajs.js.annotation.JSExportTopLevel

object ConversionTest {

  @JSExportTopLevel("UnitConversion")
  def run(): Unit = main(Array.empty[String])

  def main(args: Array[String]): Unit = {
    val temperatureConverter = unitConverter()
    document.body.replaceChild(temperatureConverter.render, document.body.firstChild)
    ()
  }

  /** Contains all supported units with their conversion from Celsius. To add support for a unit, simply add it to the
    * enum with the lens representing the corresponding conversion
    * @param lens the conversion lens from Celsius
    */
  enum TempConversion(val lens: BijectiveLens[Double, Double]):
    // All Conversions are given from Celsius
    case C extends TempConversion(new NeutralLens)
    case K extends TempConversion(new AddLens(274.15))
    case L extends TempConversion(new AddLens(253))
    case F extends TempConversion(new MulLens(1.8).compose(new AddLens(32.0)))
  end TempConversion

  /** Creates a lens which converts between any supported units */
  def conversionLens(from: TempConversion, to: TempConversion): BijectiveLens[Double, Double] =
    from.lens.inverse.compose(to.lens)

  /** A demonstration of reactive lenses using a simple unit converter for temperature units */
  def unitConverter() = {

    // Create selection for units and convert selected units to  signals
    val leftUnitInput: TypedTag[Select] = select(TempConversion.values.map { unit => option(unit.toString) })
    val (leftUnitEvent: Event[String], renderedLeftUnit: Select) =
      RenderUtil.dropDownHandler(leftUnitInput, oninput, clear = false)
    val leftUnitSignal: Signal[TempConversion] =
      leftUnitEvent.hold(init = renderedLeftUnit.value).map { TempConversion.valueOf(_) }

    val rightUnitInput: TypedTag[Select] = select(TempConversion.values.map { unit => option(unit.toString) })
    val (rightUnitEvent: Event[String], renderedRightUnit: Select) =
      RenderUtil.dropDownHandler(rightUnitInput, oninput, clear = false)
    val rightUnitSignal: Signal[TempConversion] =
      rightUnitEvent.hold(init = renderedRightUnit.value).map { TempConversion.valueOf(_) }

    // Create the two LVars containing the left and right value using reactive lenses.
    val leftVar  = LVar(0.0)
    val rightVar = leftVar.applyLens(SignalLens(Signal { conversionLens(leftUnitSignal.value, rightUnitSignal.value) }))

    // Create text fields and input events for the values
    val leftValueInput: TypedTag[Input] = input(value := leftVar.now)
    val (leftValueEvent: Event[String], renderedLeftValue: Input) =
      RenderUtil.inputFieldHandler(leftValueInput, oninput, clear = false)

    val rightValueInput: TypedTag[Input] = input(value := rightVar.now)
    val (rightValueEvent: Event[String], renderedRightValue: Input) =
      RenderUtil.inputFieldHandler(rightValueInput, oninput, clear = false)

    // Register input events as source of change for LVars
    leftVar.fire(leftValueEvent.map { toDoubleOr0(_) })
    rightVar.fire(rightValueEvent.map { toDoubleOr0(_) })

    // Observe LVars to update UI
    leftVar.observe { value => RenderUtil.setInputDisplay(renderedLeftValue, value.toString) }
    rightVar.observe { value => RenderUtil.setInputDisplay(renderedRightValue, value.toString) }

    // Combine all UI elements
    div(
      p("Unit Conversion with Lenses"),
      renderedLeftUnit,
      renderedRightUnit,
      br,
      renderedLeftValue,
      renderedRightValue
    )
  }

  /** Returns the double represented by the string or 0 if no double is represented */
  def toDoubleOr0(str: String): Double = {
    try {
      { str.toDouble }
    } catch {
      case _ => { 0.0 }
    }
  }

  /** A demonstration of the effect of declaration order on event execution if an event effects multiple LVars in the same
    * cluster. When inverting the definition of b and c, the output changes.
    */
  def raceConditionTest(): Unit = {
    val a = LVar(0)
    val b = a.applyLens(new AddLens(1).convert)
    val c = a.applyLens(new AddLens(2).convert)

    println("a is " + a.now.toString)
    println("b is " + b.now.toString)
    println("c is " + c.now.toString)

    val e = Evt[Int]()
    b.fire(e)
    c.fire(e)
    e.fire(3)
    println("\n Fire event...")
    println("a is " + a.now.toString)
    println("b is " + b.now.toString)
    println("c is " + c.now.toString)
  }

}
