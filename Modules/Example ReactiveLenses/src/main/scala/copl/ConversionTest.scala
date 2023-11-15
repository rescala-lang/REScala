package copl

import geny.Generator.from
import org.scalajs.dom.html.{Div, Input, Paragraph}
import org.scalajs.dom.{UIEvent, document, window}
import rescala.default.*
import rescala.extra.Tags.*
import scalatags.JsDom
import scalatags.JsDom.all.*
import scalatags.JsDom.{Attr, TypedTag}

object ConversionTest {

  def main(args: Array[String]): Unit = {
    val oneWayConverter = getOneWayConverter()
    document.body.replaceChild(oneWayConverter.render, document.body.firstChild)
  }

  def getOneWayConverter() = {

    val meterInput: TypedTag[Input] = input(placeholder := "Meters")
    val (meterEvent: Event[String], renderedMeter: Input) = RenderUtil.inputFieldHandler(meterInput, oninput, clear = false)

    //Q: Used to be latest
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

//Q: RenderUtil not working
//Q: LensBundle in rescala.operator
//Q: Integration of TopoBundle
